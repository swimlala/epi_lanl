CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-06-06T09:18:12Z creation;2019-06-06T09:18:15Z conversion to V3.1;2019-12-23T06:01:51Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20190606091812  20200120031517  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_148                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��h�[ 1   @��h�r @7�ȴ9X�b�Xy=�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @&ff@�  @�  A   A!��A>ffA`  A~ffA�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D{��D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D���D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�C3DɃ3D��3D�  D�C3Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�<�D� D�� D�  D�C3D� D�� D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@N{@��
@��
A	�A+�AHQ�Ai�A�(�A�(�A���A���A���A���A���A���Bz�B
z�Bz�Bz�B"z�B*z�B2z�B:z�BBz�BJz�BRz�BZz�Bbz�Bjz�Brz�Bzz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�B�C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�B�C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D.D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D.D��D'�D��D'�D��D'�D�D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D!HD��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5�HD6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt��Du'�Du��Dv'�Dv��Dw'�Dw��Dx'�Dx��Dy'�Dy��Dz'�Dz��D{'�D{��D|!HD|��D}'�D}��D~'�D~��D'�D��D��D�S�D���D���D��D�S�D���D���D��D�W
D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D�
D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D��
D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D�
D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D�D���D��D�S�DÓ�D���D��D�S�Dē�D���D��D�S�Dœ�D���D��D�S�DƓ�D���D��D�S�DǓ�D���D��D�S�Dȓ�D���D��D�W
Dɗ
D��
D��D�W
Dʓ�D���D��D�S�D˓�D���D��D�S�D̓�D���D��D�S�D͓�D���D��D�S�DΓ�D���D��D�S�Dϓ�D���D��D�S�DГ�D���D��D�S�Dѓ�D���D��D�S�Dғ�D���D��D�S�Dӓ�D���D��D�S�Dԓ�D���D��D�S�DՓ�D���D��D�S�D֓�D���D��D�S�Dד�D���D��D�S�Dؓ�D���D��D�S�Dٓ�D���D��D�S�Dړ�D���D��D�S�Dۓ�D���D��D�S�Dܓ�D���D��D�S�Dݓ�D���D��D�S�Dޓ�D���D��D�S�Dߓ�D���D��D�S�D���D���D��D�P�D��D���D��D�W
D��D���D�
D�W
D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�P�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D��
D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A���A�ĜA��A�G�A���A�E�A�I�A�=qA���A�&�A�?}A�VA�M�A��mA�I�A�+A�K�A�A��RA�/A��FA�jA�XA�M�A���A��A���A�?}A��#A��^A��A��A���A��A�A���A�VA���A�I�A�JA��A��;A���A�G�A��A��9A�7LA��HA�|�A�&�A��;A��PA�A�A���A���A�I�A���A�VA��A���A���A�-A��hA��PA�JA���A�  A���A���A���A�C�A��mA���A�ȴA���A�VA��A��A�A�A���A�E�A�-A��+A��wA�r�A~��A|��Az��Ay�7Axn�Av��Av=qAux�Arr�Ao&�Am��Al=qAj~�Ai�Af�!Ad��AcG�Aa�A`�A_��A]�A\�A["�AZ-AV�!AQ`BAO�-AM�#AJ�/AI�TAHz�AG�AF�`AF~�AGoAFv�AE�mAE��AE"�ACXABA�A@�yA?33A>Q�A<�`A<JA:�!A9��A8��A7��A7K�A6�\A4��A3l�A1\)A/�mA.�A,��A,r�A+�A+G�A*9XA)��A(E�A%�^A$��A$9XA"�A!�A �A��AA
=A5?AO�Ar�A��Al�AC�A
=AffA�AVA^5A+A�A=qA��A��A��A�7A%A|�AVA�DA��A&�A
�uA	��AJA��A
=A��AE�A�wA��A�TAS�A�A-A��A�A ȴA -@��-@��@���@�K�@�o@���@��`@��@�@�bN@��@�G�@��@�
=@�bN@��/@���@��@�ȴ@��@��m@��y@��@� �@�o@�v�@��/@�dZ@�5?@��
@�+@�"�@җ�@�hs@��/@Ѓ@Ϯ@���@�$�@�&�@ˍP@��@�x�@��@�+@�\)@�"�@Ɵ�@���@�p�@�hs@�@őh@�p�@�&�@�+@�I�@��w@���@���@��u@��P@���@�{@�hs@�j@��@���@�J@��j@��@��@�9X@�%@��9@��@�t�@�@���@���@��;@��@�b@���@��P@��F@��P@��@�1@�  @���@�33@�v�@���@�X@�&�@�Z@��m@�
=@���@���@�v�@�@�&�@���@�j@�9X@�  @��F@�dZ@�+@�
=@��!@�@�x�@�/@���@�bN@��F@�K�@�"�@��!@��\@��+@�n�@�{@�?}@��/@��@��D@�bN@��w@��P@�
=@��@���@�M�@��T@��@��@��/@�Ĝ@�j@�9X@�1@�ƨ@�|�@�\)@�K�@�33@��@��\@�V@�$�@��@��^@��7@�/@��@��/@��D@�j@�bN@�I�@��@�1@���@�|�@�o@�~�@�-@��@�@��@�G�@��/@��j@��u@���@�(�@��@��;@��@�;d@�ȴ@���@���@�~�@�{@��@��@��^@��7@�G�@���@�Q�@�b@��@��@��;@�ƨ@��@�t�@�C�@�"�@��@�o@�@��H@��H@���@�-@�J@��@���@���@�p�@�V@�%@��@��@�1@�  @��@���@���@�S�@�C�@�"�@���@��R@���@�^5@�E�@��@�@��@��^@��7@�?}@�&�@��@���@���@���@��j@��u@�I�@�b@�b@���@���@��F@��@�"�@�@�@��y@�ȴ@��R@��!@���@�v�@�E�@��@�@��^@��^@�@�x�@�V@���@��@���@�z�@�Z@�Q�@�A�@�  @��
@��F@���@�\)@�33@��@��@���@�v�@�5?@��@��@�@���@�hs@�7L@�V@��@���@�Q�@�@\)@~�y@~ff@~{@}`B@}V@|�@|9X@{�@{S�@{"�@z�@z�\@zJ@y�^@yhs@y&�@xĜ@x�@xr�@x�@x�@xbN@x �@x  @w��@w��@wK�@vȴ@v{@u�-@uO�@t�j@t�@s�F@st�@s@r�H@r��@r�\@r-@q��@q%@pQ�@o;d@n��@n�R@nE�@m�h@l�@l�@l(�@k�F@k�F@k�F@k�@kC�@k@j��@j-@i�@i��@iG�@h�9@hQ�@hb@g�w@g�@gK�@f�@e��@ep�@e/@d�@dj@ct�@c"�@b�H@b��@b^5@a��@a7L@`�`@`�@`A�@_��@_l�@_
=@^�@^��@^�+@^ff@^$�@]@]`B@]V@\�@\9X@[ƨ@[dZ@Z�H@Z�\@Z�\@Z�\@Z�\@Z~�@Z=q@ZJ@Y��@Y�#@Y�^@YG�@XĜ@XQ�@X  @W�;@W��@Wl�@WK�@V�@V��@Vv�@Vff@VE�@V$�@U�@U�-@U�h@U`B@T��@TZ@S�m@S��@S�@SdZ@S"�@S@R��@R^5@R�@Q�^@Q��@P��@P�@PbN@P  @O�P@O;d@O�@O
=@N�y@Nȴ@N�+@NV@M�T@M�-@M�h@M`B@M�@L��@L�@L��@Lj@K�m@Kt�@KC�@K@J�@J��@J~�@J-@I�#@I��@IG�@I%@Hr�@HbN@HQ�@H  @G��@Gl�@GK�@F�@F�+@FV@F{@E�@EO�@D��@DI�@D1@C�m@Cƨ@C��@CdZ@Co@B�\@BJ@A��@A�^@A��@A�7@A%@@ �@@b@@  @?�;@?��@?K�@>�+@>5?@>@=�T@=��@=�@=O�@=�@<�/@<�j@<�@<��@<z�@;ƨ@;��@;��@;dZ@:�H@:n�@:=q@:J@9��@9x�@9G�@9�@8Ĝ@8r�@8Q�@81'@7�@7�P@7|�@7|�@7\)@7+@7
=@6�y@6�@6v�@6{@5�@5�T@5@5��@5�@5p�@5/@4��@4z�@4(�@3�m@3�@3o@2�@2�H@2��@2~�@2^5@2M�@2-@1�#@1��@1G�@1%@0�u@0bN@0A�@0 �@/��@/�@/�P@/|�@/\)@/K�@/�@.ȴ@.�+@.5?@-@-p�@-?}@-�@,��@,�@,��@,�D@,j@,j@,j@,9X@+ƨ@+t�@+dZ@+o@*��@*��@*�\@*M�@)��@)G�@)%@(��@(�9@(�@(r�@(Q�@(1'@( �@(b@(  @'�@'�;@'��@';d@&��@&��@&ff@%�@%��@%p�@%?}@$�/@$��@$Z@$9X@$1@#�m@#��@#S�@#C�@#"�@"�!@"~�@"-@!�#@!�^@!�7@!hs@!&�@!%@ Ĝ@  �@   @�;@��@|�@K�@��@��@�+@v�@ff@�T@�h@`B@/@V@�j@j@�
@�F@dZ@�@�\@M�@�@��@�^@hs@G�@&�@&�@%@��@��@Ĝ@�9@��@�u@bN@ �@  @��@�@\)@+@��@�R@��@�+@V@5?@@�-@p�@?}@?}@/@��@�j@��@Z@(�@1@��@S�@33@�@�@��@��@�!@~�@-@��@�@��@��@��@��@��@x�@X@�`@��@�u@�@A�@�@�P@;d@+@
=@�R@��@�+@�+@ff@V@V@5?@�T@�@O�@/@�@V@�@��@�j@�@z�@9X@9X@(�@�@�m@ƨ@��@�@t�@S�@"�@"�@"�@o@@
�H@
�!@
~�@
�@
J@	�@	�#@	��@	�7@	�7@	�7@	x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A���A�ĜA��A�G�A���A�E�A�I�A�=qA���A�&�A�?}A�VA�M�A��mA�I�A�+A�K�A�A��RA�/A��FA�jA�XA�M�A���A��A���A�?}A��#A��^A��A��A���A��A�A���A�VA���A�I�A�JA��A��;A���A�G�A��A��9A�7LA��HA�|�A�&�A��;A��PA�A�A���A���A�I�A���A�VA��A���A���A�-A��hA��PA�JA���A�  A���A���A���A�C�A��mA���A�ȴA���A�VA��A��A�A�A���A�E�A�-A��+A��wA�r�A~��A|��Az��Ay�7Axn�Av��Av=qAux�Arr�Ao&�Am��Al=qAj~�Ai�Af�!Ad��AcG�Aa�A`�A_��A]�A\�A["�AZ-AV�!AQ`BAO�-AM�#AJ�/AI�TAHz�AG�AF�`AF~�AGoAFv�AE�mAE��AE"�ACXABA�A@�yA?33A>Q�A<�`A<JA:�!A9��A8��A7��A7K�A6�\A4��A3l�A1\)A/�mA.�A,��A,r�A+�A+G�A*9XA)��A(E�A%�^A$��A$9XA"�A!�A �A��AA
=A5?AO�Ar�A��Al�AC�A
=AffA�AVA^5A+A�A=qA��A��A��A�7A%A|�AVA�DA��A&�A
�uA	��AJA��A
=A��AE�A�wA��A�TAS�A�A-A��A�A ȴA -@��-@��@���@�K�@�o@���@��`@��@�@�bN@��@�G�@��@�
=@�bN@��/@���@��@�ȴ@��@��m@��y@��@� �@�o@�v�@��/@�dZ@�5?@��
@�+@�"�@җ�@�hs@��/@Ѓ@Ϯ@���@�$�@�&�@ˍP@��@�x�@��@�+@�\)@�"�@Ɵ�@���@�p�@�hs@�@őh@�p�@�&�@�+@�I�@��w@���@���@��u@��P@���@�{@�hs@�j@��@���@�J@��j@��@��@�9X@�%@��9@��@�t�@�@���@���@��;@��@�b@���@��P@��F@��P@��@�1@�  @���@�33@�v�@���@�X@�&�@�Z@��m@�
=@���@���@�v�@�@�&�@���@�j@�9X@�  @��F@�dZ@�+@�
=@��!@�@�x�@�/@���@�bN@��F@�K�@�"�@��!@��\@��+@�n�@�{@�?}@��/@��@��D@�bN@��w@��P@�
=@��@���@�M�@��T@��@��@��/@�Ĝ@�j@�9X@�1@�ƨ@�|�@�\)@�K�@�33@��@��\@�V@�$�@��@��^@��7@�/@��@��/@��D@�j@�bN@�I�@��@�1@���@�|�@�o@�~�@�-@��@�@��@�G�@��/@��j@��u@���@�(�@��@��;@��@�;d@�ȴ@���@���@�~�@�{@��@��@��^@��7@�G�@���@�Q�@�b@��@��@��;@�ƨ@��@�t�@�C�@�"�@��@�o@�@��H@��H@���@�-@�J@��@���@���@�p�@�V@�%@��@��@�1@�  @��@���@���@�S�@�C�@�"�@���@��R@���@�^5@�E�@��@�@��@��^@��7@�?}@�&�@��@���@���@���@��j@��u@�I�@�b@�b@���@���@��F@��@�"�@�@�@��y@�ȴ@��R@��!@���@�v�@�E�@��@�@��^@��^@�@�x�@�V@���@��@���@�z�@�Z@�Q�@�A�@�  @��
@��F@���@�\)@�33@��@��@���@�v�@�5?@��@��@�@���@�hs@�7L@�V@��@���@�Q�@�@\)@~�y@~ff@~{@}`B@}V@|�@|9X@{�@{S�@{"�@z�@z�\@zJ@y�^@yhs@y&�@xĜ@x�@xr�@x�@x�@xbN@x �@x  @w��@w��@wK�@vȴ@v{@u�-@uO�@t�j@t�@s�F@st�@s@r�H@r��@r�\@r-@q��@q%@pQ�@o;d@n��@n�R@nE�@m�h@l�@l�@l(�@k�F@k�F@k�F@k�@kC�@k@j��@j-@i�@i��@iG�@h�9@hQ�@hb@g�w@g�@gK�@f�@e��@ep�@e/@d�@dj@ct�@c"�@b�H@b��@b^5@a��@a7L@`�`@`�@`A�@_��@_l�@_
=@^�@^��@^�+@^ff@^$�@]@]`B@]V@\�@\9X@[ƨ@[dZ@Z�H@Z�\@Z�\@Z�\@Z�\@Z~�@Z=q@ZJ@Y��@Y�#@Y�^@YG�@XĜ@XQ�@X  @W�;@W��@Wl�@WK�@V�@V��@Vv�@Vff@VE�@V$�@U�@U�-@U�h@U`B@T��@TZ@S�m@S��@S�@SdZ@S"�@S@R��@R^5@R�@Q�^@Q��@P��@P�@PbN@P  @O�P@O;d@O�@O
=@N�y@Nȴ@N�+@NV@M�T@M�-@M�h@M`B@M�@L��@L�@L��@Lj@K�m@Kt�@KC�@K@J�@J��@J~�@J-@I�#@I��@IG�@I%@Hr�@HbN@HQ�@H  @G��@Gl�@GK�@F�@F�+@FV@F{@E�@EO�@D��@DI�@D1@C�m@Cƨ@C��@CdZ@Co@B�\@BJ@A��@A�^@A��@A�7@A%@@ �@@b@@  @?�;@?��@?K�@>�+@>5?@>@=�T@=��@=�@=O�@=�@<�/@<�j@<�@<��@<z�@;ƨ@;��@;��@;dZ@:�H@:n�@:=q@:J@9��@9x�@9G�@9�@8Ĝ@8r�@8Q�@81'@7�@7�P@7|�@7|�@7\)@7+@7
=@6�y@6�@6v�@6{@5�@5�T@5@5��@5�@5p�@5/@4��@4z�@4(�@3�m@3�@3o@2�@2�H@2��@2~�@2^5@2M�@2-@1�#@1��@1G�@1%@0�u@0bN@0A�@0 �@/��@/�@/�P@/|�@/\)@/K�@/�@.ȴ@.�+@.5?@-@-p�@-?}@-�@,��@,�@,��@,�D@,j@,j@,j@,9X@+ƨ@+t�@+dZ@+o@*��@*��@*�\@*M�@)��@)G�@)%@(��@(�9@(�@(r�@(Q�@(1'@( �@(b@(  @'�@'�;@'��@';d@&��@&��@&ff@%�@%��@%p�@%?}@$�/@$��@$Z@$9X@$1@#�m@#��@#S�@#C�@#"�@"�!@"~�@"-@!�#@!�^@!�7@!hs@!&�@!%@ Ĝ@  �@   @�;@��@|�@K�@��@��@�+@v�@ff@�T@�h@`B@/@V@�j@j@�
@�F@dZ@�@�\@M�@�@��@�^@hs@G�@&�@&�@%@��@��@Ĝ@�9@��@�u@bN@ �@  @��@�@\)@+@��@�R@��@�+@V@5?@@�-@p�@?}@?}@/@��@�j@��@Z@(�@1@��@S�@33@�@�@��@��@�!@~�@-@��@�@��@��@��@��@��@x�@X@�`@��@�u@�@A�@�@�P@;d@+@
=@�R@��@�+@�+@ff@V@V@5?@�T@�@O�@/@�@V@�@��@�j@�@z�@9X@9X@(�@�@�m@ƨ@��@�@t�@S�@"�@"�@"�@o@@
�H@
�!@
~�@
�@
J@	�@	�#@	��@	�7@	�7@	�7@	x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B}�B}�B}�B}�B}�B}�B}�B}�B|�B{�Bv�Bm�BcTBw�B�DB��B�wBŢB��B�B�BS�BffBw�B�B�B�B�B�B�+B�=B�PB�DBu�BiyBcTBffBx�B�JB��B�bB�+B�Bw�BdZBR�B;dB1'B.B,B(�B,B-B,B)�B%�B'�B2-B=qB;dB7LB33B,B)�B+B1'BA�BD�B6FB#�B�BuBB��B��B�B��B�jB�XB�?B�B��B��B�Br�BT�B1'B�B{B1B
�B
��B
�!B
u�B
I�B
:^B
+B
 �B
�B
\B
	7B
B	�B	��B	��B	�?B	��B	��B	�+B	s�B	dZB	YB	O�B	G�B	:^B	.B	$�B	�B��BɺB�RB�B��B��B��B��B��B�B��B�BB�NB�TB�TB�B��BÖB�RB�'B�B��B��B��B��B��B��B��B��B�\B�+B� B{�Bu�Bs�Bq�Bo�Bm�Bk�Bk�BhsBe`BcTBcTB_;BaHB`BB\)B[#B[#B\)B]/B\)B]/B]/B]/B_;B_;B]/B^5B\)B_;B[#BR�BH�B<jB;dB:^B1'B.B.B.B+B(�B,B,B(�B)�B'�B%�B&�B'�B$�B#�B#�B"�B"�B!�B �B!�B#�B"�B!�B!�B �B �B!�B�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B(�B$�B#�B"�B"�B"�B"�B%�B%�B%�B'�B(�B(�B(�B(�B(�B/B/B2-B6FB6FB5?B49B49B7LB=qBA�BE�BE�BC�B;dB;dB<jB;dB=qB?}B?}B@�BB�BC�BE�BF�BG�BM�BO�BQ�BT�BZB`BBffBhsBiyBiyBjBn�Br�Bv�Bw�Bw�B{�B�B�B�1B�1B�7B�PB�PB�hB�uB��B�{B��B��B��B��B��B��B��B�B�-B�9B�?B�LB�^B�qB�wB��BŢBɺB��B��B��B��B�B�)B�BB�HB�HB�HB�ZB�B�B�B�B��B��B��B	B	+B		7B	JB	\B	oB	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	&�B	+B	.B	/B	1'B	33B	5?B	7LB	:^B	:^B	<jB	?}B	@�B	@�B	A�B	C�B	E�B	H�B	L�B	L�B	O�B	S�B	XB	YB	ZB	[#B	aHB	bNB	cTB	dZB	gmB	k�B	l�B	n�B	n�B	n�B	p�B	r�B	t�B	x�B	y�B	y�B	z�B	{�B	~�B	�B	�+B	�=B	�DB	�JB	�JB	�VB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�-B	�3B	�?B	�FB	�RB	�XB	�^B	�dB	�qB	�}B	�}B	B	B	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�;B	�BB	�BB	�BB	�BB	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
B
B
B
B
B
%B
1B
1B
1B
	7B
	7B
	7B

=B
DB
DB
JB
JB
JB
PB
PB
VB
VB
VB
PB
PB
PB
PB
PB
PB
PB
PB
PB
\B
\B
\B
\B
bB
hB
hB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
D�B
E�B
E�B
F�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B}�B}�B}�B}�B}�B}�B}�B}�B|�B{�Bv�Bm]Bc Bw�B�B��B�BB�mBϫB�cB�BS�Bf2Bw�B��B��B��B��B��B��B�	B�B�Bu�BiDBc Bf2Bx�B�B�_B�.B��B��Bw�Bd&BR�B;0B0�B-�B+�B(�B+�B,�B+�B)�B%�B'�B1�B=<B;0B7B2�B+�B)�B*�B0�BA;BDgB5�B#�BKB&B�B��B��B�iBѷB�6B�$B�B��B�qB�2B��Br|BT�B0�B�B,B�B
�iB
�~B
��B
u�B
I�B
:B
*�B
 �B
eB
(B
�B
�B	�CB	ϑB	�UB	�B	��B	�eB	��B	s�B	d&B	X�B	O�B	GzB	:B	-�B	$�B	eB��BɆB�B��B��B�xB�~B��B��B��BбB��B�B�B� B��B�rB�GB�B��B��B��B��B��B�|B�jB�]B�QB�9B�B��B�B{�ButBshBqvBoOBm]BkQBkQBh$BeBcBcB_B`�B_�B[�BZ�BZ�B[�B\�B[�B\�B\�B\�B^�B^�B\�B]�B[�B^�BZ�BR�BHfB<B;B:*B0�B-�B-�B-�B*�B(�B+�B+�B(�B)�B'�B%�B&�B'�B$�B#�B#�B"�B"�B!|B vB!|B#�B"�B!�B!|B �B vB!|BpB!|B!|BdBjBjBdBpBKBEBEB]BWBQBYB_BQBdBpB(�B$�B#�B"�B"�B"�B"�B%�B%�B%�B'�B(�B(�B(�B(�B(�B.�B.�B1�B5�B5�B4�B3�B3�B6�B="BA;BESBESBCGB;0B;0B<B;B="B?.B?.B@4BBABCGBESBFYBGEBM�BO�BQ�BT�BY�B_�BfBh$BiBi*Bj0BnIBraBvzBw�Bw�B{�B��B��B��B��B��B��B�B�B�&B�2B�,B�2B�EB�KB�dB�pB��B��B��B��B��B��B��B�B�"B�(B�B�SB�lB�xBΊBЗBөB��B��B��B��B��B��B��B�CB�[B�aB�hB�tB��B��B	�B	�B	�B	�B	B	 B	2B	?B	EB	WB	]B	dB	pB	!|B	#nB	$�B	&�B	*�B	-�B	.�B	0�B	2�B	4�B	6�B	:B	:B	<B	?.B	@4B	@4B	A;B	CGB	ESB	HKB	L~B	L~B	O�B	S�B	W�B	X�B	Y�B	Z�B	`�B	a�B	cB	c�B	gB	k6B	l=B	nIB	nIB	nIB	pUB	raB	tnB	x�B	y�B	yrB	zxB	{B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�9B	�9B	�?B	�$B	�EB	�QB	�dB	�VB	��B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�"B	�B	�B	�'B	�'B	�GB	�SB	�_B	�rB	�xB	�xB	�~B	�jB	̈́B	ΊB	�vB	уB	҉B	ңB	ԯB	յB	ּB	רB	��B	خB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�$B	�*B	�*B	�*B	�0B	�6B	�6B	�=B	�CB	�IB	�5B	�UB	�UB	�[B	�AB	�GB	�hB	�hB	�hB	�nB	�tB	�`B	�`B	�fB	��B	��B	��B	��B	�lB	��B	�rB	�rB	�rB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B

�B
�B
�B
�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
 B
B
B
,B
,B
2B
2B
2B
B
B
$B
?B
EB
EB
KB
1B
WB
WB
WB
=B
WB
WB
]B
]B
]B
]B
dB
jB
VB
pB
 vB
 vB
 vB
!|B
!bB
!|B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#nB
#�B
#nB
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
0�B
0�B
0�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
4�B
4�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
8B
8B
8B
8B
8B
9	B
9	B
9	B
8�B
:B
:B
:B
;B
;B
;B
;B
<B
<B
="B
="B
="B
="B
>B
>(B
>(B
>(B
>(B
?.B
?.B
?.B
?.B
@4B
@4B
@B
@4B
A;B
A;B
A B
A;B
A;B
A;B
BAB
BAB
CGB
BAB
B'B
BAB
CGB
CGB
CGB
CGB
CGB
CGB
CGB
D3B
DMB
ESB
DMB
ESB
ESB
FYB
ESB
ESB
ESB
F?B
FYB
FYB
F?B
G_B
GEB
HfB
HfB
IRB
IlB
IlB
IRB
IlB
IRB
IlB
JrB
JrB
JrB
JrB
JrB
JrB
JrB
KxB
KxB
KxB
KxB
L~B
L~B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
OvB
O�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
aB
`�B
a�B
a�B
a�B
bB
bB
a�B
a�B
a�B
a�B
a�B
cB
cB
cB
cB
cB
b�B
cB
dB
d&B
dB
dB
d&B
dB
c�B
dB
d�B
eB
eB
eB
eB
eB
eB
e�B
fB
fB
fB
fB
fB
gB
h$B
h$B
h
B
h$B
h$B
h$B
h
B
i*B
iB
i*B
i*B
jB
i*B
i*B
j0B
j0B
j0B
j0B
j0B
j0B
j0B
k6B
k6B
kB
k6B
k6B
k6B
kB
l=B
l=B
l=B
l=B
lWB
l"B
l=B
mCB
m]B
mCB
m]B
m)B
m)B
m]B
mCB
mCB
mCB
mCB
mCB
nIB
nIB
nIB
nIB
nIB
n/B
ncB
nIB
n/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.62(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905300036132019053000361320190530003613201905310031432019053100314320190531003143JA  ARFMdecpA19c                                                                20190606181449  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190606091812  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190606091813  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190606091814  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190606091814  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190606091814  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190606091814  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190606091814  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190606091815                      G�O�G�O�G�O�                JA  ARUP                                                                        20190606101515                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190525153617  CV  JULD            G�O�G�O�F�A                JM  ARCAJMQC2.0                                                                 20190529153613  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190529153613  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190530153143  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120031517                      G�O�G�O�G�O�                