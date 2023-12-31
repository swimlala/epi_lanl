CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:30Z AOML 3.0 creation; 2016-05-31T19:14:32Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230530  20160531121432  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               .A   AO  4051_7090_046                   2C  D   APEX                            5368                            041511                          846 @֧X�̀1   @֧YAl 
@5���F�d�O�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    .A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�fD�I�D���D���D�	�D�I�D���D�� D�� D�6fD�y�D��fD�fD�<�DږfD�ɚD�3D�0 D�y�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @|��@�  @�  A  A<  A\  A|  A�  A�  A�  A�  A�  A���A�  A�  B  B  B  B��B'  B/  B7  B?  BG  BO  BW  B_  Bg  Bo  Bw  B  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BÀ Bǀ Bˀ Bπ BӀ B׀ Bۀ B߀ B� B� B� B� B� B�� B�� B�� C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D p D � Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D	p D	� D
p D
� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D�Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%� D&p D&� D'p D'� D(p D(� D)p D)� D*p D*� D+p D+� D,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3p D3� D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD�fDEp DE� DFp DF� DGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRp DR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^p D^� D_p D_� D`p D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh� Dip Di� Djp Dj� Dkp Dk� Dlp Dl� Dmp Dm� Dnp Dn� Dop Do� Dpp Dp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt�3Dy��D��fD�A�D���D���D��D�A�D���D�� D�� D�.fD�q�D��fD��fD�4�DڎfD���D��3D�( D�q�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A���Aԡ�A�C�A�&�A�JA�%A���A��#AӾwA�x�A�\)A�/A�JA���A��mA���Aҝ�A�I�A�l�AζFA��A�1A�ƨA�ȴA��Aº^A���A�;dA�33A��A�9XA�A��-A�oA��uA���A�G�A�
=A��jA�ZA���A��jA�K�A�A���A�~�A���A�\)A��yA��RA�l�A�(�A��mA�G�A�l�A�&�A�A��A��A��^A���A�r�A�7LA���A�5?A�JA��7A�oA���A�z�A��;A��\A��wA�I�A��/A��wA���A�?}A��+A�$�A�bA��DA�Q�A���A�ffA��^A���A��A��;A��-A��\A�ƨA��A��yA�p�A�JA���A��wA���A�jA�$�A�x�A�ffA��A�O�A��yA��-A���A�|�A�~�A�-A��A��;A�VA~$�Az�HAw�
AvjAu��At�Ar�AqXAp(�Ao&�An��AkXAj�RAhv�Af��AfI�Ae/AdI�Aa�AaG�Aa%A`=qA]`BAZffAX��ATJAR9XAQ�AN��ALv�AKS�AH��AG�AG%AF�+AF(�AD�AC;dAA�mA?\)A;��A:�`A:�RA:{A8E�A7�FA6�9A5��A4=qA3�A2�HA1l�A0�jA/
=A-��A-dZA,�A+��A*�A)VA(5?A'��A'�A'%A%O�A"�`A�#AO�A^5A�A��AA�A$�A�A�AXA�A"�AVA�7A{AJA�A(�A��Al�AbNA��A+A�A
�`A�RA�A+AĜAVA�AA+A�!AA�A��AAx�A%@���@�"�@�-@���@�+@��m@���@��@�hs@�9X@�S�@�M�@���@��@�bN@�ƨ@�hs@�h@�\)@⟾@�^5@��@�?}@�l�@�~�@�$�@ݺ^@݉7@�Ĝ@�$�@ّh@�&�@ם�@֟�@�G�@ӝ�@щ7@У�@Ͼw@�o@ΰ!@��@��@��m@ˮ@�S�@�n�@ɡ�@�O�@ȋD@Ǯ@���@Ɨ�@��@��@��@î@�E�@��^@�O�@�j@��
@��@�ƨ@���@�33@���@���@��7@�/@�1@���@��@��@���@�ff@�&�@��@�r�@� �@��
@��@�E�@��-@�x�@��@�z�@�9X@���@��H@�5?@�n�@�=q@�x�@��@��@���@��!@�V@��@��#@���@�1@�|�@�dZ@�"�@���@�5?@���@�x�@��@��@���@���@��@�C�@�n�@��^@�`B@�G�@�7L@���@���@��D@���@�dZ@�K�@�@�@�hs@��@�X@�G�@�?}@�?}@�?}@�?}@�7L@��@���@��u@� �@�1@�b@�1@���@��F@�dZ@�o@��@��R@��+@�{@��#@�@�G�@�%@��@���@���@�%@�&�@�X@�p�@��@�G�@��T@��@�{@���@�x�@�Z@��@��@�o@�ȴ@�"�@��F@���@��`@��h@�?}@���@���@��/@���@�%@�%@��@��@� �@���@��@�\)@�33@�+@��@��y@��R@���@�V@��@��7@�G�@�/@�/@�&�@��@��@��@��@��@��@��`@�j@��;@�dZ@�;d@�o@��@�^5@�J@���@��7@�&�@���@��D@�1'@��
@�C�@��@���@���@�v�@�5?@���@���@�`B@���@��9@�9X@���@��@�dZ@�@��y@��@�ȴ@�v�@�-@�@��@�X@��@��@��@���@�j@� �@�|�@�33@���@���@�ff@�E�@�$�@�J@��@��#@�bN@�A�@z�!@o�w@fV@\�@T��@O�@G��@@�u@<I�@5?}@.��@*�@"�H@`B@�@�@b@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�K�A���Aԡ�A�C�A�&�A�JA�%A���A��#AӾwA�x�A�\)A�/A�JA���A��mA���Aҝ�A�I�A�l�AζFA��A�1A�ƨA�ȴA��Aº^A���A�;dA�33A��A�9XA�A��-A�oA��uA���A�G�A�
=A��jA�ZA���A��jA�K�A�A���A�~�A���A�\)A��yA��RA�l�A�(�A��mA�G�A�l�A�&�A�A��A��A��^A���A�r�A�7LA���A�5?A�JA��7A�oA���A�z�A��;A��\A��wA�I�A��/A��wA���A�?}A��+A�$�A�bA��DA�Q�A���A�ffA��^A���A��A��;A��-A��\A�ƨA��A��yA�p�A�JA���A��wA���A�jA�$�A�x�A�ffA��A�O�A��yA��-A���A�|�A�~�A�-A��A��;A�VA~$�Az�HAw�
AvjAu��At�Ar�AqXAp(�Ao&�An��AkXAj�RAhv�Af��AfI�Ae/AdI�Aa�AaG�Aa%A`=qA]`BAZffAX��ATJAR9XAQ�AN��ALv�AKS�AH��AG�AG%AF�+AF(�AD�AC;dAA�mA?\)A;��A:�`A:�RA:{A8E�A7�FA6�9A5��A4=qA3�A2�HA1l�A0�jA/
=A-��A-dZA,�A+��A*�A)VA(5?A'��A'�A'%A%O�A"�`A�#AO�A^5A�A��AA�A$�A�A�AXA�A"�AVA�7A{AJA�A(�A��Al�AbNA��A+A�A
�`A�RA�A+AĜAVA�AA+A�!AA�A��AAx�A%@���@�"�@�-@���@�+@��m@���@��@�hs@�9X@�S�@�M�@���@��@�bN@�ƨ@�hs@�h@�\)@⟾@�^5@��@�?}@�l�@�~�@�$�@ݺ^@݉7@�Ĝ@�$�@ّh@�&�@ם�@֟�@�G�@ӝ�@щ7@У�@Ͼw@�o@ΰ!@��@��@��m@ˮ@�S�@�n�@ɡ�@�O�@ȋD@Ǯ@���@Ɨ�@��@��@��@î@�E�@��^@�O�@�j@��
@��@�ƨ@���@�33@���@���@��7@�/@�1@���@��@��@���@�ff@�&�@��@�r�@� �@��
@��@�E�@��-@�x�@��@�z�@�9X@���@��H@�5?@�n�@�=q@�x�@��@��@���@��!@�V@��@��#@���@�1@�|�@�dZ@�"�@���@�5?@���@�x�@��@��@���@���@��@�C�@�n�@��^@�`B@�G�@�7L@���@���@��D@���@�dZ@�K�@�@�@�hs@��@�X@�G�@�?}@�?}@�?}@�?}@�7L@��@���@��u@� �@�1@�b@�1@���@��F@�dZ@�o@��@��R@��+@�{@��#@�@�G�@�%@��@���@���@�%@�&�@�X@�p�@��@�G�@��T@��@�{@���@�x�@�Z@��@��@�o@�ȴ@�"�@��F@���@��`@��h@�?}@���@���@��/@���@�%@�%@��@��@� �@���@��@�\)@�33@�+@��@��y@��R@���@�V@��@��7@�G�@�/@�/@�&�@��@��@��@��@��@��@��`@�j@��;@�dZ@�;d@�o@��@�^5@�J@���@��7@�&�@���@��D@�1'@��
@�C�@��@���@���@�v�@�5?@���@���@�`B@���@��9@�9X@���@��@�dZ@�@��y@��@�ȴ@�v�@�-@�@��@�X@��@��@��@���@�j@� �@�|�@�33@���@���@�ff@�E�@�$�@�J@��@��#@�bN@�A�@z�!@o�w@fV@\�@T��@O�@G��@@�u@<I�@5?}@.��@*�@"�H@`B@�@�@b@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B�uB�bB�1B�%B�B}�B|�B|�Bz�Bx�Br�Bl�BgmB`BB1'B49BVB`BBI�BP�B\)BcTBffBhsBm�BjBaHBbNBq�Bo�Bu�B�1B�+Bz�Bo�BjBo�Bl�B�%B�uB��B��B��B��B��B�{B�DB� B}�B|�B{�Bz�Bw�Bv�Bt�Br�Bl�BhsBffB_;BS�BH�B49B+B$�B�BuBDB	7B��B�yB�TB��B��B�FB��B�Bk�B]/BE�B"�BB��B�LB��Bw�BffB^5BXBR�BP�BN�BJ�BD�B9XB'�B�B%B
�B
�5B
��B
�dB
��B
}�B
jB
\)B
R�B
?}B
,B
�B
VB
+B	��B	�B	�fB	�)B	��B	��B	��B	�jB	�B	��B	��B	��B	�oB	�B	�B	� B	z�B	jB	T�B	C�B	�B	+B	B��B��B��B	{B	{B	oB	bB	PB	
=B	B��B�B�ZB�BB�;B�)B��B�B�B�B�5B�NB�`B�NB�;B�B��B��B��B��BĜB�}B�jB�^B�XB�?B�B��B��B��B�{B�hB�\B�VB�VB�PB�JB�7B�B�B|�Bx�Bu�Bq�Bm�BjBgmBe`BcTBaHB^5B[#BXBVBVBVBW
BW
BW
BW
BXBYBYBYB[#B_;B_;BaHBaHBaHBaHBaHBdZBe`Be`Be`Be`Be`BcTBbNB`BB_;B^5B`BBe`BffBffBe`Be`Be`BhsBjBjBk�BjBiyBm�Bn�Bm�Bn�Bl�Bm�Bm�Bn�Bo�Bo�Bo�Bp�Bs�Bx�B|�B� B�B�%B�1B�7B�DB�\B�oB��B��B��B��B��B��B��B��B��B��B��B�B�3B�3B�9B�9B�^B�^B�wB��BÖBȴB��B��B�B�B�B�/B�5B�/B�B�
B�B�B�B�`B�mB�fB�sB�yB�B�yB�sB�mB�mB�mB�mB�fB�`B�B�B�B�B��B��B	B	+B	DB	PB	PB	PB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	'�B	)�B	+B	0!B	8RB	>wB	@�B	E�B	H�B	J�B	J�B	J�B	J�B	K�B	K�B	L�B	N�B	Q�B	R�B	R�B	R�B	S�B	VB	W
B	W
B	XB	YB	[#B	[#B	\)B	_;B	aHB	ffB	k�B	o�B	r�B	t�B	v�B	x�B	�B	�B	�1B	�=B	�=B	�=B	�JB	�JB	�DB	�DB	�JB	�oB	��B	��B	��B	�B	�3B	�-B	�'B	�'B	�3B	�FB	�RB	�dB	�dB	�dB	�qB	�wB	�wB	�wB	�}B	��B	��B	��B	ÖB	ÖB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�)B	�/B	�/B	�;B	�BB	�HB	�NB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
JB
oB
�B
%�B
0!B
5?B
8RB
:^B
@�B
F�B
K�B
Q�B
W
B
^5B
bNB
e`B
iyB
m�B
q�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B�|B�KB�CB�!B~B}B}
Bz�Bx�Br�Bl�Bg�B`ZB1AB4QBVB`ZBI�BP�B\BBcqBf�Bh�Bm�Bj�BaaBbhBq�Bo�Bu�B�NB�GBz�Bo�Bj�Bo�Bl�B�AB��B��B��B��B��B��B��B�`B�B~B}B| Bz�Bw�Bv�Bt�Br�Bl�Bh�Bf�B_VBTBH�B4UB+B$�B�B�B]B	PB�B�B�oB��B��B�_B��B�9Bk�B]HBE�B"�BB��B�aB��Bw�Bf}B^MBX+BS	BP�BN�BJ�BD�B9pB(B�B>B
�B
�QB
��B
��B
�B
~B
j�B
\EB
SB
?�B
,'B
�B
wB
LB	� B	��B	�B	�LB	�B	��B	��B	��B	�(B	��B	��B	��B	��B	�DB	�4B	�'B	{B	j�B	U'B	C�B	�B	WB	0B�B�&B� B	�B	�B	�B	�B	|B	
iB	IB�!B��B�B�qB�kB�UB�B�AB�=B�MB�bB�~B�B�}B�kB�EB�&B�B�B��B��B��B��B��B��B�pB�@B�	B��B��B��B��B��B��B��B��B�{B�iB�KB�;B}!ByBu�Bq�Bm�Bj�Bg�Be�Bc�Ba}B^kB[XBXEBV:BV9BV9BWABW>BW?BWABXFBYLBYLBYMB[YB_oB_qBa}Ba~Ba}Ba~Ba�Bd�Be�Be�Be�Be�Be�Bc�Bb�B`xB_nB^kB`yBe�Bf�Bf�Be�Be�Be�Bh�Bj�Bj�Bk�Bj�Bi�Bm�Bn�Bm�Bn�Bl�Bm�Bm�Bn�Bo�Bo�Bo�Bp�Bs�By
B}#B�7B�9B�YB�dB�kB�yB��B��B��B��B��B��B��B�B�*B�0B�/B�0B�!B�5B�gB�dB�lB�mB��B��B��B��B��B��B��B�	B�AB�AB�OB�aB�hB�`B�OB�;B�4B�7B�MB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B�B	6B	ZB	rB	�B	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%B	(B	**B	+0B	0OB	8~B	>�B	@�B	E�B	H�B	J�B	J�B	J�B	J�B	K�B	K�B	L�B	OB	RB	S B	SB	SB	T%B	V2B	W5B	W6B	X<B	YEB	[PB	[OB	\TB	_gB	atB	f�B	k�B	o�B	r�B	t�B	v�B	yB	�2B	�<B	�YB	�iB	�iB	�gB	�tB	�sB	�pB	�nB	�uB	��B	��B	��B	�B	�=B	�\B	�VB	�QB	�OB	�^B	�oB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	üB	ÿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�"B	�&B	�-B	�1B	�?B	�JB	�QB	�XB	�VB	�eB	�iB	�oB	�uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�"B	�"B
 &B
 'B
4B
oB
�B
�B
&
B
0IB
5bB
8yB
:�B
@�B
F�B
K�B
RB
W.B
^YB
bsB
e�B
i�B
m�B
q�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.25 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214322016053112143220160531121432  AO  ARCAADJP                                                                    20140721230530    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230530  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230530  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121432  IP                  G�O�G�O�G�O�                