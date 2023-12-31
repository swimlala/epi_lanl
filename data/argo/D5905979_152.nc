CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:30Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170930  20220204114426  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�~�f1   @�`�V@5���n��bڰ ě�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�
D�)�D�`�D��
D�� D�'\D�X D��HD��=D��D�Y�D���D�ɚD�)�D�X Dڗ�D���D��D�P�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @|��@���@�  A  A<  A\  A|  A�  A�  A�  A�  A�  A�  A�  A�  B  B  BffB  B'  B/  B7  B?  BG  BO  BW  B_  Bg  Bo  Bw  B  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BÀ Bǀ Bˀ Bπ BӀ B׀ Bۀ B߀ B� B� B� B� B� B�� B�� B�� C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C��3C�� C�� C�� C�� C�� D p D � Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D	p D	� D
p D
� Dp D� Dp D� Dp D� Dp D� Dp D�fDp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%� D&p D&� D'p D'� D(p D(� D)p D)� D*p D*� D+p D+� D,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3p D3� D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD� DEp DE� DFp DF� DGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRp DR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^p D^� D_p D_� D`p D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh�Dip Di� Djp Dj� Dkp Dk� Dlp Dl� Dmp Dm� Dnp Dn� Dop Do� Dpp Dp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt�3Dy�
D�!�D�X�D��
D�� D�\D�P D��HD��=D��D�Q�D���D���D�!�D�P Dڏ�D���D��D�H�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aş�Aş�Aş�Aŗ�Ař�Ař�AœuAœuAŗ�Aŗ�Aŗ�Ař�A�x�A�;dA��A��
A��;A�M�A���A�oA���A��A�1'A�(�A���A�oA��9A�S�A�O�A�oA���A���A�33A��A�JA�r�A�9XA��#A��9A��A�$�A�{A�|�A��A���A�JA��+A��A���A�G�A��A��A�A�1'A��A��wA�^5A���A�JA���A���A��jA�ĜA�ĜA�|�A��A��+A��!A���A�E�A�A�A�33A�
=A��TA�\)A��/A�"�A�|�A��^A�JA�{A�r�A�A�O�A��A��HA��jA�z�A�$�A��yA���A��uA�p�A�G�A��RA���A�I�A�1'A��A��TA���A�1A�~�A��A��+A��A��#A�oA�E�A��A���A�M�A�ȴA��A���A�ZA~�+A{;dAx~�Av1Aq�7Ao7LAmS�Akt�Ah5?Ae|�Ac�Aa�A`5?A_�A\~�AW�AS�mAQ��AQS�AP��AN=qAH��AF�AE��AE"�AD(�AC%ABQ�A@��A?�FA>�jA=hsA;S�A9l�A7x�A6�DA5oA3�A2�RA1O�A0�A/�A.(�A.1A-��A+�TA+O�A*r�A(�/A'A&Q�A$��A"ĜA"JA!x�A �RAp�A��A��AJA�PA�A�\A7LA1'A�9A$�A�AoA��Av�A�Ap�A�+AjAQ�A{Al�AZA��A�A�
A
ĜA
Q�A	��A�!AoA�A33A�A �A �y@�ff@��@�Z@��\@�p�@�V@��@�(�@�+@�+@��`@@�o@�R@�ff@��@�hs@���@�Q�@��;@��H@�@�j@�
=@�-@��m@�-@�^@�O�@�bN@�+@���@�x�@�Ĝ@�1@�\)@ڰ!@؛�@�n�@�{@���@�;d@��#@�V@Ь@�(�@ϕ�@�o@�^5@�{@�G�@�A�@�S�@ʟ�@��@��/@�\)@Ɨ�@�v�@�{@�@�A�@�v�@���@�?}@��@���@��m@��F@�dZ@�dZ@�+@�ȴ@���@�?}@�Q�@��@�@�X@��D@��
@���@�J@��@��@�S�@���@��T@��-@���@�/@��D@� �@��@���@��@�/@��j@�r�@�I�@���@���@��@�^5@��T@���@���@���@��-@�r�@���@�dZ@��@���@��\@�E�@���@�/@�z�@�  @�l�@�
=@�ȴ@�n�@��@�J@��@��^@�/@��`@��j@�r�@�I�@��;@�t�@�;d@�+@���@�-@���@��@���@�X@��@��@�Z@�1'@�ƨ@���@�t�@�S�@��@���@���@�v�@�^5@�V@�V@�=q@�{@��T@���@��^@�G�@�%@��9@�j@�Q�@�(�@�  @���@��@���@�33@�@�ȴ@��!@��+@��@�@��@�`B@�O�@��@���@�j@�A�@�r�@�r�@�9X@�1@��F@�l�@��!@�@���@��7@���@��@�hs@��u@�I�@�  @�  @��@�;d@��H@��@���@���@�x�@��9@��@�Q�@�b@���@���@�+@���@��\@�~�@�n�@�=q@��T@���@�hs@�?}@��@���@��/@���@��@�9X@��;@��w@��@�t�@���@���@��@��\@�$�@��@���@�p�@�`B@��@��`@���@���@�j@�I�@�1'@��@���@��F@���@�dZ@�C�@��@��@��R@���@���@���@���@��!@���@�~�@�^5@�=q@�5?@�@���@�p�@��@���@��/@��9@��u@�r�@}��@w�@o�}@gE9@_g�@X[�@T �@L�$@F��@?y�@:�@2e@-o @)�@#\)@@��@5?@@�*@(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aş�Aş�Aş�Aŗ�Ař�Ař�AœuAœuAŗ�Aŗ�Aŗ�Ař�A�x�A�;dA��A��
A��;A�M�A���A�oA���A��A�1'A�(�A���A�oA��9A�S�A�O�A�oA���A���A�33A��A�JA�r�A�9XA��#A��9A��A�$�A�{A�|�A��A���A�JA��+A��A���A�G�A��A��A�A�1'A��A��wA�^5A���A�JA���A���A��jA�ĜA�ĜA�|�A��A��+A��!A���A�E�A�A�A�33A�
=A��TA�\)A��/A�"�A�|�A��^A�JA�{A�r�A�A�O�A��A��HA��jA�z�A�$�A��yA���A��uA�p�A�G�A��RA���A�I�A�1'A��A��TA���A�1A�~�A��A��+A��A��#A�oA�E�A��A���A�M�A�ȴA��A���A�ZA~�+A{;dAx~�Av1Aq�7Ao7LAmS�Akt�Ah5?Ae|�Ac�Aa�A`5?A_�A\~�AW�AS�mAQ��AQS�AP��AN=qAH��AF�AE��AE"�AD(�AC%ABQ�A@��A?�FA>�jA=hsA;S�A9l�A7x�A6�DA5oA3�A2�RA1O�A0�A/�A.(�A.1A-��A+�TA+O�A*r�A(�/A'A&Q�A$��A"ĜA"JA!x�A �RAp�A��A��AJA�PA�A�\A7LA1'A�9A$�A�AoA��Av�A�Ap�A�+AjAQ�A{Al�AZA��A�A�
A
ĜA
Q�A	��A�!AoA�A33A�A �A �y@�ff@��@�Z@��\@�p�@�V@��@�(�@�+@�+@��`@@�o@�R@�ff@��@�hs@���@�Q�@��;@��H@�@�j@�
=@�-@��m@�-@�^@�O�@�bN@�+@���@�x�@�Ĝ@�1@�\)@ڰ!@؛�@�n�@�{@���@�;d@��#@�V@Ь@�(�@ϕ�@�o@�^5@�{@�G�@�A�@�S�@ʟ�@��@��/@�\)@Ɨ�@�v�@�{@�@�A�@�v�@���@�?}@��@���@��m@��F@�dZ@�dZ@�+@�ȴ@���@�?}@�Q�@��@�@�X@��D@��
@���@�J@��@��@�S�@���@��T@��-@���@�/@��D@� �@��@���@��@�/@��j@�r�@�I�@���@���@��@�^5@��T@���@���@���@��-@�r�@���@�dZ@��@���@��\@�E�@���@�/@�z�@�  @�l�@�
=@�ȴ@�n�@��@�J@��@��^@�/@��`@��j@�r�@�I�@��;@�t�@�;d@�+@���@�-@���@��@���@�X@��@��@�Z@�1'@�ƨ@���@�t�@�S�@��@���@���@�v�@�^5@�V@�V@�=q@�{@��T@���@��^@�G�@�%@��9@�j@�Q�@�(�@�  @���@��@���@�33@�@�ȴ@��!@��+@��@�@��@�`B@�O�@��@���@�j@�A�@�r�@�r�@�9X@�1@��F@�l�@��!@�@���@��7@���@��@�hs@��u@�I�@�  @�  @��@�;d@��H@��@���@���@�x�@��9@��@�Q�@�b@���@���@�+@���@��\@�~�@�n�@�=q@��T@���@�hs@�?}@��@���@��/@���@��@�9X@��;@��w@��@�t�@���@���@��@��\@�$�@��@���@�p�@�`B@��@��`@���@���@�j@�I�@�1'@��@���@��F@���@�dZ@�C�@��@��@��R@���@���@���@���@��!@���@�~�@�^5@�=q@�5?@�@���@�p�@��@���@��/@��9@��uG�O�@}��@w�@o�}@gE9@_g�@X[�@T �@L�$@F��@?y�@:�@2e@-o @)�@#\)@@��@5?@@�*@(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
bB
�B
L�B
e`B
_;B
^5B
dZB
l�B
v�B
�\B
�B
�LB
�^B
��B
�yB
�B
��B�B�B�B$�B7LBI�BC�BL�B\)By�B�\B��B��B��B��B��B�B�-B�9B�LB�jBB��B��B�B�/B�HB�mB�B�B��B��BB �B�B�B�B&�B=qBZB\)B_;B`BBk�Bq�Bl�BaHB^5BbNB^5BB�B-B%�B�BVBDB+B��B�ZB�
BǮB��B�PB�DB|�BdZBR�BD�B;dB/BuB+B
��B
�mB
�B
ƨB
�dB
�B
��B
�\B
x�B
bNB
Q�B
?}B
)�B
�B
B	�fB	��B	�9B	��B	��B	�+B	w�B	`BB	T�B	D�B	:^B	49B	#�B	1B�B�)B�B��BŢB�FB��B��B��B��B�{B�bB�PB�DB�DB�B�B{�Bv�Bq�Bm�Bk�Bq�Bn�Bk�BffBaHB`BB_;B^5BVBVBT�BM�BK�BL�BG�BE�BB�BA�BA�BA�B?}BA�B?}B?}B>wB?}B;dB>wB9XB8RB8RB8RB6FB;dB>wB>wB=qB=qB=qB<jB<jB;dB:^B8RB6FB5?B33B0!B,B(�B(�B&�B$�B!�B$�B%�B%�B(�B'�B&�B%�B'�B%�B%�B&�B%�B%�B%�B%�B%�B%�B%�B%�B$�B%�B%�B%�B&�B%�B)�B)�B)�B)�B+B-B-B-B.B.B/B/B2-B49B33B5?B7LB8RB9XB:^B:^B;dB;dB=qB<jB=qB?}B@�BB�BB�BD�BI�BK�BK�BN�BM�BQ�BXB[#B[#B]/B_;B_;B`BB`BB`BB`BBaHBbNBdZBgmBjBm�Bo�Br�Bt�Bx�By�B}�B�B�%B�1B�VB�\B�\B�bB�oB�oB�bB�oB��B��B��B��B��B�B�B�9B�^B��BBBBƨB��B��B�B�B�)B�HB�ZB�yB�B�B�B��B��B��B	B	B	B	B	+B	DB	PB	\B	bB	hB	{B	�B	�B	�B	�B	!�B	#�B	%�B	'�B	,B	-B	/B	2-B	33B	7LB	9XB	@�B	B�B	E�B	G�B	I�B	K�B	L�B	L�B	L�B	M�B	P�B	S�B	VB	W
B	ZB	[#B	^5B	aHB	cTB	ffB	iyB	k�B	n�B	r�B	u�B	w�B	x�B	y�B	z�B	~�B	�B	�B	�B	�B	�B	�B	�B	�7B	�JB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�LB	�RB	�RB	�XB	�XB	�jB	�qB	�}B	B	B	ĜB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�/B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B
�B
�B
!�B
)*B
2�B
=�B
B�B
I�B
MB
T�B
YB
\�B
a�B
f�B
l�B
o�B
r-B
yrB
|6B
.111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
9B
=sB
VB
O�B
N�B
T�B
]/B
glB
�B
��B
��B
��B
�xB
�B
�?B
�B	#B<BHBlB'�B:EB4"B=XBL�BjaB�B�B�4B�5B�YB�}B��B��B��B��B��B�B�cB�oBʚBͫB��B��B��B�B�CB�gB��B:B4B
B)B_B-�BJ�BL�BO�BP�B[�BbB\�BQ�BN�BR�BN�B3B�B\B	B��B��B��B�[B��BǏB�6B�uB}�B{�Bm�BT�BC�B56B+�B�BB
��B
�B
�B
ȹB
�SB
�B
��B
�iB
�B
i�B
SB
B�B
0;B
�B
PB	��B	�0B	¹B	�B	��B	�iB	xB	h�B	Q B	E�B	5~B	+AB	%B	�B�B�B�B�B��B��B�=B��B��B��B��B�wB�_B~NB|BB|BBuBtBl�Bg�Bb�B^�B\�Bb�B_�B\�BWlBROBQIBPBBO=BGBGBFB>�B<�B=�B8�B6�B3�B2�B2�B2�B0�B2�B0�B0�B/�B0�B,tB/�B*iB)cB)cB)cB'XB,vB/�B/�B.�B.�B.�B-|B-|B,wB+qB)eB'ZB&SB$HB!6BBBB B�B�B�B�B�BB	BB�B
B�B�BB�B�B�B�B�B�B�B�B�B�B�B�BB�BBBBBB*B*B*B0B0B 7B 7B#IB%UB$OB&[B(hB)nB*tB+zB+zB,�B,�B.�B-�B.�B0�B1�B3�B3�B5�B:�B<�B<�B?�B>�BCBI*BL=BL=BNIBPUBPUBQ\BQ\BQ\BQ\BRbBShBUtBX�B[�B^�B`�Bc�Be�Bi�Bj�BoBs#Bw<ByHBlB�rB�rB�xB��B��B�xB��B��B��B��B�B�B�B�(B�LB�pB��B��B��B��B��B�B�B�B�,B�8B�VB�hBڇBݘB�B��B��B��B��B�B�$B�$B�*B�6B�NB�ZB	 fB	lB	rB	�B	�B		�B	�B	�B	�B	�B	�B	�B	B	B	 "B	#3B	$9B	(RB	*^B	1�B	3�B	6�B	8�B	:�B	<�B	=�B	=�B	=�B	>�B	A�B	D�B	GB	HB	KB	L%B	O7B	RJB	TUB	WgB	ZzB	\�B	_�B	c�B	f�B	h�B	i�B	j�B	k�B	o�B	s
B	tB	s
B	tB	uB	uB	vB	z4B	}GB	�YB	�eB	�kB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�,B	�8B	�EB	�KB	�KB	�QB	�QB	�cB	�jB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�+B	�1B	�8B	�8B	�8B	�=B	�CB	�IB	�IB	�OB	�OB	�OB	�UB	�[B	�hB	�hB	�tB	�yB	ޅB	ޅB	ߌB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�G�O�B	�B	�uB
�B
�B
B
#�B
.�B
3�B
:�B
>B
E�B
JlB
M�B
R�B
W�B
]�B
`�B
cB
j]B
m!B
p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.25 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0002), vertically averaged dS =-0.015(+/-0.006) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144262022020411442620220204114426  AO  ARCAADJP                                                                    20200619170930    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170930  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170930  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114426  IP                  G�O�G�O�G�O�                