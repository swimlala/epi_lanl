CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:01Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170901  20220204114414  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               &A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؒ�0��1   @ؒ���R@8CS����c��;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    &A   B   B   @�  @���A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3y�D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�&D�Y�D���D��RD�3D�X�D��\D��D�%�D�aHD��D���D�{D�R�D�R�D���D�fD�]�D�D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @���@�  A  A<  A\  A|  A�33A�  A�  A�  A�  A�  A�  A���B  B  B  B  B'  B/  B7  B?  BG  BO  BW  B_  Bg  Bn��Bw  B  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BÀ Bǀ Bˀ Bϳ3BӀ B׀ Bۀ B߀ B� B� B� B� B� B�� B�� B�� C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D p D � Dp D� Dp D�fDp D� Dp D� Dp D� Dp D� Dp D� Dp D� D	p D	� D
p D
� Dp D� Dp D� Dp D�Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%� D&p D&� D'p D'� D(p D(� D)p D)� D*p D*� D+p D+� D,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3i�D3�D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD� DEp DE� DFp DF� DGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRp DR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^p D^� D_p D_� D`p D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh� Dip Di� Djp Dj� Dkp Dk� Dlp Dl� Dmp Dm� Dnp Dn� Dop Do� Dpp Dp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt��Dy��D�D�Q�D���D��RD�3D�P�D�\D��D��D�YHD��D���D�{D�J�D�J�D���D�fD�U�D�D�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bNA�jA�hsA�l�A�jA�l�A�n�A�r�A�~�A�~�AʁAʁAʃAʅAʃAʃAʅAʃAʁAʁAʁAʃAʃAʃAʁAʅAʇ+AʋDAʉ7A�z�A�~�AʃA�|�A�r�A�jA�G�A�&�A�ƨA���A�p�A�9XA��-A�
=A�hsA��A��-A��/A��A�M�A�A�A��wA�=qA�C�A��A���A�K�A�1'A��
A�^5A�ZA��A�/A��A��TA��A��A�r�A���A� �A��A�$�A��^A��!A���A��A�K�A���A�O�A�A���A�33A���A�K�A���A��A�bA��
A���A��RA��RA�z�A���A��DA���A�^5A���A�33A��A��wA�x�A�?}A�\)A��A��A��\A��#A��A�z�A��A�ffA�t�A�A�K�A��;A�=qA�C�A�\)A���A� �AoA}dZAy�Aw;dAtE�Ar�uAo�mAm�AkƨAh�9Ae��Ae��Ad-AbA�A]��AX��AV��AR��AQx�AP~�AN�!AN{ALM�AJ��AI;dAG�AFĜAE�-AD�\AB��A@�HA>��A=&�A<{A:�DA8Q�A7��A6��A5|�A3�A2��A2�A2n�A2ZA2$�A1�PA0��A.��A-;dA,r�A+�A+K�A*�RA*ZA(�RA'��A'+A&�jA%��A%��A%XA%VA$z�A#��A"��A"ffA"JA!?}A ��A A�A��AXA��AZA�TA�A�!A
=A  A�mAO�A��A1'A\)AĜA��A&�A��AdZA�-A��A�AZA��A+A
��A
�/A
�!A	��A�A�^AA^5A1'A�;At�AG�A��A�\A�A ��@��@�V@��@�E�@�-@��T@���@�&�@��D@�|�@�  @���@�@�r�@��@���@��@�?}@�j@���@睲@�33@柾@��@�(�@��@� �@�33@���@��#@܃@���@ٺ^@�Q�@�@�&�@�bN@� �@�ƨ@�dZ@�n�@�X@Л�@��@�S�@��y@�V@��@���@ʰ!@�@��`@Ǿw@�v�@Ų-@�G�@�Ĝ@�j@��@¸R@�=q@�@�hs@�z�@��@��y@�-@�7L@�r�@���@��+@�`B@���@��@�^5@��@�I�@���@���@��#@���@�x�@��@�j@��w@���@�^5@���@��h@�%@���@�bN@��F@���@���@��@���@�hs@�j@�|�@�;d@��@��!@�V@�$�@�@��-@���@�x�@��h@��^@��u@�C�@�;d@���@�n�@��@��^@�@�O�@���@�v�@�v�@�\)@��y@�\)@�o@���@�/@�G�@�V@���@���@���@���@�@���@��+@�V@�5?@�$�@��@�p�@���@�1'@��@�9X@��/@�j@�  @�+@���@�~�@���@��@��@���@�ȴ@�o@�@���@���@�-@�5?@�v�@�V@�M�@��@���@��-@���@�hs@��/@���@�A�@�\)@��y@���@��!@���@��+@���@��@��H@�@�@���@�p�@�?}@�/@�V@�V@���@�Ĝ@��9@�r�@��;@��P@�;d@���@��@���@��@�o@��@��R@�ȴ@��@��#@�O�@��w@�K�@��@���@��y@��H@��@�K�@��;@��F@��F@�dZ@��H@�=q@�Ĝ@���@��j@��@�X@�x�@���@��@��@�&�@�7L@�%@���@��@��9@�z�@�bN@�b@���@��;@��F@�S�@��!@��\@�5?@�@���@��h@��7@�p�@�hs@�`B@�X@�X@�X@���@~��@u+�@i@d�@Zq�@Tj@K�@@G�A@?� @8�@0@*�F@$��@!�"@�@�@��@:�@~�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�bNA�jA�hsA�l�A�jA�l�A�n�A�r�A�~�A�~�AʁAʁAʃAʅAʃAʃAʅAʃAʁAʁAʁAʃAʃAʃAʁAʅAʇ+AʋDAʉ7A�z�A�~�AʃA�|�A�r�A�jA�G�A�&�A�ƨA���A�p�A�9XA��-A�
=A�hsA��A��-A��/A��A�M�A�A�A��wA�=qA�C�A��A���A�K�A�1'A��
A�^5A�ZA��A�/A��A��TA��A��A�r�A���A� �A��A�$�A��^A��!A���A��A�K�A���A�O�A�A���A�33A���A�K�A���A��A�bA��
A���A��RA��RA�z�A���A��DA���A�^5A���A�33A��A��wA�x�A�?}A�\)A��A��A��\A��#A��A�z�A��A�ffA�t�A�A�K�A��;A�=qA�C�A�\)A���A� �AoA}dZAy�Aw;dAtE�Ar�uAo�mAm�AkƨAh�9Ae��Ae��Ad-AbA�A]��AX��AV��AR��AQx�AP~�AN�!AN{ALM�AJ��AI;dAG�AFĜAE�-AD�\AB��A@�HA>��A=&�A<{A:�DA8Q�A7��A6��A5|�A3�A2��A2�A2n�A2ZA2$�A1�PA0��A.��A-;dA,r�A+�A+K�A*�RA*ZA(�RA'��A'+A&�jA%��A%��A%XA%VA$z�A#��A"��A"ffA"JA!?}A ��A A�A��AXA��AZA�TA�A�!A
=A  A�mAO�A��A1'A\)AĜA��A&�A��AdZA�-A��A�AZA��A+A
��A
�/A
�!A	��A�A�^AA^5A1'A�;At�AG�A��A�\A�A ��@��@�V@��@�E�@�-@��T@���@�&�@��D@�|�@�  @���@�@�r�@��@���@��@�?}@�j@���@睲@�33@柾@��@�(�@��@� �@�33@���@��#@܃@���@ٺ^@�Q�@�@�&�@�bN@� �@�ƨ@�dZ@�n�@�X@Л�@��@�S�@��y@�V@��@���@ʰ!@�@��`@Ǿw@�v�@Ų-@�G�@�Ĝ@�j@��@¸R@�=q@�@�hs@�z�@��@��y@�-@�7L@�r�@���@��+@�`B@���@��@�^5@��@�I�@���@���@��#@���@�x�@��@�j@��w@���@�^5@���@��h@�%@���@�bN@��F@���@���@��@���@�hs@�j@�|�@�;d@��@��!@�V@�$�@�@��-@���@�x�@��h@��^@��u@�C�@�;d@���@�n�@��@��^@�@�O�@���@�v�@�v�@�\)@��y@�\)@�o@���@�/@�G�@�V@���@���@���@���@�@���@��+@�V@�5?@�$�@��@�p�@���@�1'@��@�9X@��/@�j@�  @�+@���@�~�@���@��@��@���@�ȴ@�o@�@���@���@�-@�5?@�v�@�V@�M�@��@���@��-@���@�hs@��/@���@�A�@�\)@��y@���@��!@���@��+@���@��@��H@�@�@���@�p�@�?}@�/@�V@�V@���@�Ĝ@��9@�r�@��;@��P@�;d@���@��@���@��@�o@��@��R@�ȴ@��@��#@�O�@��w@�K�@��@���@��y@��H@��@�K�@��;@��F@��F@�dZ@��H@�=q@�Ĝ@���@��j@��@�X@�x�@���@��@��@�&�@�7L@�%@���@��@��9@�z�@�bN@�b@���@��;@��F@�S�@��!@��\@�5?@�@���@��h@��7@�p�@�hs@�`B@�X@�XG�O�@���@~��@u+�@i@d�@Zq�@Tj@K�@@G�A@?� @8�@0@*�F@$��@!�"@�@�@��@:�@~�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBJ�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BG�BI�BI�BI�BJ�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BH�BH�BE�BP�BT�BYB\)B^5B_;B`BBgmBk�Bl�Bn�Bs�Br�Bs�Bv�Bv�Bw�Bw�Bw�Bw�By�Bz�Bz�By�Bx�Bx�Bz�Bz�B}�B}�B|�B|�B{�Bz�Bv�Bo�Bk�BffB\)BP�BK�BF�B@�B8RB2-B-B!�B\B
=B��B�B�HB�)B��BƨB�B��B��B��B�oB�+B�B{�Bm�B[#B@�B-BPB
��B
�BB
ÖB
�dB
�B
��B
��B
��B
��B
�PB
~�B
t�B
jB
]/B
M�B
,B
�B
hB	��B	�mB	�B	ĜB	�B	�JB	�JB	�1B	u�B	N�B	uB	+B�`B�mB�sB�HB�fB�B��B��B�LB�-B�B�?B�B��B��B��B�\B�DB�B�B}�B~�Bs�Bu�B� B~�B�=B��B��B��B�VB�B�B�B�B�B�+B�1B�B�B~�B}�B{�Bz�Bx�Bw�Bt�Bq�Bp�Bo�Bm�Bk�BiyBiyBgmBffBe`BdZBbNBbNB_;B]/B\)B\)BZB[#BXBVBR�BN�BK�BI�BG�BG�BF�BF�BF�BD�BD�BC�BB�BD�B?}B?}B?}B=qB=qB=qB<jB<jB;dB:^B;dB9XB7LB8RB7LB8RB:^B:^B:^B;dB:^B:^B9XB7LB7LB6FB6FB6FB5?B49B5?B7LB7LB6FB6FB6FB6FB7LB:^B8RB9XB9XB:^B<jB<jB=qB@�BB�BB�BB�BB�BB�BD�BF�BF�BG�BH�BI�BJ�BN�BO�BR�BT�BVBYB\)B^5B^5B`BBaHBbNBe`BffBffBhsBjBk�Bn�Bo�Bs�Bu�Bv�Bz�B}�B�B�B�+B�=B�\B�bB�{B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�9B�?B�dB�jB�wB��B��BƨBȴB��B��B��B��B��B�
B�B�B�HB�fB�yB�B��B��B��B	  B	%B	%B	  B��B��B	+B		7B	uB	{B	$�B	/B	2-B	49B	1'B	.B	-B	0!B	5?B	6FB	7LB	7LB	7LB	8RB	8RB	9XB	>wB	?}B	A�B	J�B	R�B	S�B	R�B	R�B	R�B	T�B	W
B	R�B	T�B	W
B	ZB	`BB	dZB	dZB	e`B	hsB	jB	m�B	q�B	q�B	s�B	t�B	u�B	u�B	u�B	v�B	w�B	w�B	y�B	{�B	|�B	}�B	}�B	}�B	~�B	�B	�B	�B	� B	� B	� B	�B	�B	�B	�B	�B	�1B	�7B	�=B	�=B	�=B	�=B	�=B	�DB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	�{B	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�!B	�'B	�3B	�RB	�^B	�wB	�}B	�}B	�}B	��B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�B	�*B
tB
�B
�B
IB
(
B
./B
6+B
?B
E�B
M6B
S�B
X�B
]�B
`�B
f�B
jeB
oOB
s�B
x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BBCBA<BA<BA<BA<BA<BA<BA<BA<BA<BA<BA<BA<BA<BA<BA<BA<BA<BA<B?0BA<BA<BA<BBCBA<BA<BA<BA<BA<BA<BA<BA<BA<BA<BA<B@6B@6B=%BHhBL�BP�BS�BU�BV�BW�B^�Bc	BdBfBk:Bj4Bk:BnMBnNBoTBoTBoTBoTBq`BrfBrfBq`BpZBpZBrgBrgBuzBuzBttBttBsmBrgBnPBg&BcB]�BS�BHoBCQB>3B8B/�B)�B$�BYB�B�B�B�*B��BӼBˌB�=B��B�oB�DB� B�B~�Bx�Bs�Be-BR�B8"B$�B�B
��B
��B
�?B
�B
��B
��B
�^B
�XB
�:B
��B
v�B
llB
b/B
T�B
E�B
#�B
nB
	 B	��B	�(B	��B	�YB	��B	�B	�B	�B	m�B	F�B	@B��B�/B�<B�BB�B�5B��BŤB�UB�B�B��B�B��B��B��B�XB�3B�B|�Bx�Bu�Bv�Bk�Bm�Bw�Bv�B�B�xB�kB�YB�/B|�B|�B{�B{�By�BB�By�Bx�Bv�Bu�Bs�Br�Bp�Bo�Bl�Bi�Bh�Bg{BenBcbBaWBaWB_KB^DB]>B\8BZ,BZ-BWBUBTBTBQ�BSBO�BM�BJ�BF�BC�BA�B?�B?�B>�B>�B>�B<B<B;yB:rB<B7aB7aB7aB5UB5UB5UB4NB4NB3IB2CB3IB1=B/2B08B/2B08B2DB2DB2DB3JB2DB2DB1>B/3B/3B.-B.-B.-B-&B, B-&B/3B/3B..B..B..B..B/4B2FB0:B1@B1@B2FB4RB4RB5YB8kB:wB:wB:wB:wB:wB<�B>�B>�B?�B@�BA�BB�BF�BG�BJ�BL�BM�BP�BTBVBVBX*BY0BZ6B]HB^NB^NB`ZBbfBclBfBg�Bk�Bm�Bn�Br�Bu�By�B}BB�#B�BB�HB�aB�sB�sB�yB��B��B��B��B��B��B��B��B� B�B�B�B�$B�HB�NB�[B�gB�mB��B��BŶB��B��B��B��B��B��B� B�*B�HB�[B�zB��B��B��B��B�B�B��B��B��B�B	B	UB	[B	�B	&�B	*
B	,B	)B	%�B	$�B	'�B	-B	.#B	/)B	/)B	/)B	0/B	0/B	15B	6TB	7ZB	9fB	B�B	J�B	K�B	J�B	J�B	J�B	L�B	N�B	J�B	L�B	N�B	Q�B	XB	\5B	\5B	];B	`NB	bZB	elB	i�B	i�B	k�B	l�B	m�B	m�B	m�B	n�B	o�B	o�B	q�B	s�B	t�B	u�B	u�B	u�B	v�B	x�B	z�B	y�B	w�B	w�B	w�B	y�B	{�B	{�B	{�B	|�B	�
B	�B	�B	�B	�B	�B	�B	�B	�/B	�HB	�TB	�ZB	�`B	�rB	�fB	�fB	�lB	�TB	�NB	�ZB	�sB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�)B	�5B	�MB	�SB	�SB	�SB	�YB	�lB	�~B	��B	B	B	B	B	ũB	ƯB	ǵB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�iB	��B	�GB
~B
TB
B
�B
&B
-�B
6�B
=sB
EB
K�B
PcB
U�B
X`B
^QB
b5B
gB
kkB
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.25 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144142022020411441420220204114414  AO  ARCAADJP                                                                    20200619170901    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170901  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170901  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114414  IP                  G�O�G�O�G�O�                