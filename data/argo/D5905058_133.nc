CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-03-21T18:36:57Z creation;2019-03-21T18:37:00Z conversion to V3.1;2019-12-23T06:05:05Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190321183657  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_133                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ذej���1   @ذf""" @8�U�=��c4�TɅ�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ Dټ�D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��fD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�
=@��
A	�A)�AI�Ai�A���A���A���A���A���A���A���A���Bz�B
z�Bz�Bz�B"z�B*z�B2z�B:z�BBz�BJz�BRz�BZz�Bbz�Bj{Brz�Bzz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd�Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D.D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt��Du'�Du��Dv'�Dv��Dw'�Dw��Dx'�Dx��Dy'�Dy��Dz'�Dz��D{'�D{��D|'�D|��D}'�D}��D~'�D~��D'�D��D��D�S�D���D���D��D�S�D���D���D��D�S�D��
D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�P�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D�D���D��D�S�DÓ�D���D��D�S�Dē�D���D��D�S�Dœ�D���D��D�S�DƓ�D���D��D�S�DǓ�D���D��D�S�Dȓ�D���D��D�S�Dɓ�D���D��D�S�Dʓ�D���D��D�S�D˓�D���D��D�S�D̓�D���D��D�S�D͓�D���D��D�S�DΓ�D���D��D�S�Dϓ�D���D��D�S�DГ�D���D��D�S�Dѓ�D���D��D�S�Dғ�D���D��D�S�Dӓ�D���D��D�S�Dԓ�D���D��D�S�DՓ�D���D��D�S�D֓�D���D��D�S�Dד�D���D��D�S�Dؓ�D���D��D�S�Dٓ�D�ФD��D�S�Dړ�D���D��D�S�Dۓ�D���D��D�S�Dܓ�D���D��D�S�Dݓ�D���D��D�S�Dޓ�D���D��D�S�Dߓ�D���D��D�S�D���D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D�
D���D��D�P�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D��
D��=D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�S�A�M�A�G�A�%A�=qA�(�A��A�ĜA��PA�v�A�dZA�O�A�K�A�I�A�G�A�E�A�C�A�A�A�A�A�C�A�E�A�$�A�bA��`A���A�dZA�K�A���A�-A�+A��FA��^A���A�hsA��A��A�hsA���A��!A�=qA���A���A�ȴA�ĜA��
A��A��A��#A�~�A��
A���A�\)A��A��uA�-A��-A�jA�/A���A�r�A��A��+A��wA�1A��FA��yA�E�A��
A�p�A�=qA��
A�5?A��DA�O�A�33A���A���A�\)A��A��A���A�C�A���A���A�x�A��A�XA��A�7LA���A���A�C�A��
A��A���A�Q�A�r�A���A��A�bA��DA���A�/A�O�A�`BA��A��A��7A��
A�A�ZA�I�A�hA~�A~  AxjAu�-At�\As\)Ar�Ap��An�Aj�`AidZAd�+Aa�A^�!A[`BAZ�AY%AXjAX9XAW\)AUS�AS�ARA�AQ+AN�jAL~�ALbAK�7AJ�`AI&�AF�AFAE/AC�#AA�#A=
=A;K�A:1A7��A5l�A5�PA4ĜA3�hA3K�A2��A2jA1G�A0��A/��A-�A,��A+O�A(�A'��A'7LA'�PA'`BA&�HA&1'A$�A#�FA"�yA"E�A!�FA!C�A!
=A ��A bA �AoA�/A�9A�Ap�A��An�A��AA~�A��A�/AjAA�/AJA�AC�A
jA	ƨA	�PA	"�A
=Ap�A7LAȴA�A=qA��A�A�A {@�r�@���@�V@���@��u@�dZ@�hs@�@��#@@�@�dZ@�J@�bN@���@�@��@�
=@�5?@��@ߥ�@�dZ@��@ޗ�@�@ޗ�@�V@��@�ƨ@�"�@���@�v�@��m@���@��@֗�@Ցh@���@ԣ�@��m@�dZ@���@��@�/@� �@�~�@��`@�=q@ț�@�|�@��H@�&�@�A�@öF@�@�@�-@���@�\)@��@�^5@���@��7@���@��
@�@���@�j@�1'@�o@���@���@��/@�Q�@�|�@�v�@�J@�`B@�9X@��m@�S�@�ȴ@�@�Ĝ@�(�@���@�;d@��!@��@��#@�O�@��@�Ĝ@��@�Z@�j@�Z@�A�@�C�@�
=@��!@�M�@�{@���@�G�@���@���@�Z@�t�@�"�@��y@���@�=q@��@��-@�p�@�`B@�G�@�%@��`@��@�bN@�ƨ@�dZ@���@��+@�V@�$�@�@��T@��-@�hs@���@�1@���@�C�@�
=@���@�~�@�{@��@��^@�G�@�V@���@��/@�z�@� �@�b@�1@��;@��P@�33@��@���@��+@�$�@��@���@��-@���@��7@�hs@�`B@�7L@���@��@�9X@�(�@�  @��;@���@�|�@�C�@�33@��H@���@�V@�-@�=q@��@��@�hs@�G�@��D@�r�@�bN@�Z@�ƨ@���@�t�@�\)@�S�@�C�@�o@�
=@�@�
=@��y@��@��@���@���@��@��@��T@���@��-@��7@�7L@���@���@��9@��@���@��u@�r�@�I�@��@���@��P@�|�@�S�@�o@���@�n�@�-@�E�@�J@��@�G�@���@��@�/@��@��@�V@��@���@�Ĝ@��u@�j@�I�@��@��;@��w@�t�@�+@�+@�o@�
=@��@��H@���@�5?@�{@��@��-@��-@���@���@��7@�O�@��@�r�@�I�@�9X@���@�|�@�
=@��H@��R@��!@�v�@�{@��T@���@��^@���@�hs@�/@���@��`@���@��@�bN@�b@�P@
=@~ȴ@~��@~@}�T@}?}@|�@|�D@|�@{�
@{t�@{@z~�@z�@y�@y7L@x��@x��@x�@x1'@w�;@wl�@w+@v�y@v��@vV@v@u�@u?}@t��@t��@t��@tI�@s�m@s�@s@rn�@q��@qX@pĜ@pA�@o�w@o;d@n�@nff@n{@m��@m�h@l��@l�@k�m@k�@k"�@k@j�!@jJ@i�^@i��@ix�@i�@h�`@h�@hbN@hQ�@g�w@g;d@f�y@f��@fV@fE�@f5?@f{@e�@e�T@e@e�h@eV@dZ@c��@cƨ@c�@c33@b��@b=q@ax�@a7L@a�@`Ĝ@`��@`r�@`bN@`A�@`1'@`b@_�@_;d@^��@^�+@^V@^$�@]��@]p�@]/@\�@\Z@\�@[��@["�@Z�!@Zn�@Z�@Y��@Yx�@Y&�@X�@X  @W�;@W�;@W�w@W��@W�P@Wl�@W\)@W;d@W
=@Vȴ@Vff@V5?@V$�@U�@U��@Up�@U�@T�@Tz�@S��@Sƨ@S��@S"�@R�@R�!@R��@Rn�@RJ@Q��@Q�@Q�#@Q�^@Q��@Q�7@QX@P�`@P��@P�9@PbN@PA�@O�@O��@OK�@N��@Nȴ@N��@M�T@Mp�@M/@L��@L�D@Lj@L(�@Kƨ@Kt�@K"�@J��@Jn�@J^5@JM�@J-@J�@JJ@I��@I�@I�#@I��@I�^@Ix�@I�@H��@H�@Hr�@HQ�@G�@G�@G+@G�@F�y@Fȴ@F�R@Fff@FV@F5?@F{@E@E�h@Ep�@Ep�@E`B@E?}@D�/@D��@DI�@D1@Ct�@CC�@C@B�!@B~�@B=q@Ahs@A&�@A%@@��@@��@@�u@@A�@?�;@?�w@?+@>�@>��@>ff@>E�@>$�@=�@=��@=��@=p�@=?}@<j@;�m@;ƨ@;�@;t�@;S�@;S�@:�@:n�@9��@9�#@9��@9x�@97L@8Ĝ@8�9@8�u@81'@7�@7��@7;d@6�@6V@5O�@4�/@4I�@3��@3��@3dZ@3S�@3"�@2��@2��@2~�@2=q@2J@2J@1�#@1�^@1�^@1��@1x�@1X@1�@0��@0�`@0��@0 �@0  @0  @/�@/
=@.�y@.ȴ@.V@.E�@.@-��@-�@-?}@-V@,��@,�D@,9X@,9X@,(�@,�@+��@+t�@+C�@*�!@)��@)�^@)��@(��@(�9@(��@(�u@(�u@(�@(�@(bN@( �@(  @'�w@'�@'��@'\)@'�@&��@&�+@&V@&{@%�T@%@%p�@%/@$�/@$�@$z�@$9X@#��@#��@#��@#�@#dZ@#S�@"�@"�\@"n�@"=q@"�@"J@!��@!��@!%@ ��@ �`@ �`@ ��@ ��@ Ĝ@ Ĝ@ Ĝ@ �9@ ��@ r�@ b@�;@��@��@|�@\)@K�@K�@
=@�@�+@ff@5?@{@�@O�@/@�@V@��@�/@�j@��@z�@j@1@��@t�@S�@33@��@M�@�@J@�@�#@�#@��@x�@7L@&�@�@�@�@��@�@Q�@ �@�;@�;@��@�w@l�@�@�y@��@ff@@�-@p�@�/@��@�D@�D@z�@9X@��@�F@�F@��@�@dZ@"�@��@^5@-@�@J@�@�#@�^@��@�7@X@G�@7L@�@��@�`@��@��@bN@A�@  @�;@�P@K�@�@ȴ@�R@��@��@�+@�+@ff@$�@�@��@�-@�h@`B@V@�j@z�@Z@(�@�
@�F@�F@C�@@
��@
�!@
^5@
=q@
-@
J@	�@	�^@	�7@	7L@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�S�A�M�A�G�A�%A�=qA�(�A��A�ĜA��PA�v�A�dZA�O�A�K�A�I�A�G�A�E�A�C�A�A�A�A�A�C�A�E�A�$�A�bA��`A���A�dZA�K�A���A�-A�+A��FA��^A���A�hsA��A��A�hsA���A��!A�=qA���A���A�ȴA�ĜA��
A��A��A��#A�~�A��
A���A�\)A��A��uA�-A��-A�jA�/A���A�r�A��A��+A��wA�1A��FA��yA�E�A��
A�p�A�=qA��
A�5?A��DA�O�A�33A���A���A�\)A��A��A���A�C�A���A���A�x�A��A�XA��A�7LA���A���A�C�A��
A��A���A�Q�A�r�A���A��A�bA��DA���A�/A�O�A�`BA��A��A��7A��
A�A�ZA�I�A�hA~�A~  AxjAu�-At�\As\)Ar�Ap��An�Aj�`AidZAd�+Aa�A^�!A[`BAZ�AY%AXjAX9XAW\)AUS�AS�ARA�AQ+AN�jAL~�ALbAK�7AJ�`AI&�AF�AFAE/AC�#AA�#A=
=A;K�A:1A7��A5l�A5�PA4ĜA3�hA3K�A2��A2jA1G�A0��A/��A-�A,��A+O�A(�A'��A'7LA'�PA'`BA&�HA&1'A$�A#�FA"�yA"E�A!�FA!C�A!
=A ��A bA �AoA�/A�9A�Ap�A��An�A��AA~�A��A�/AjAA�/AJA�AC�A
jA	ƨA	�PA	"�A
=Ap�A7LAȴA�A=qA��A�A�A {@�r�@���@�V@���@��u@�dZ@�hs@�@��#@@�@�dZ@�J@�bN@���@�@��@�
=@�5?@��@ߥ�@�dZ@��@ޗ�@�@ޗ�@�V@��@�ƨ@�"�@���@�v�@��m@���@��@֗�@Ցh@���@ԣ�@��m@�dZ@���@��@�/@� �@�~�@��`@�=q@ț�@�|�@��H@�&�@�A�@öF@�@�@�-@���@�\)@��@�^5@���@��7@���@��
@�@���@�j@�1'@�o@���@���@��/@�Q�@�|�@�v�@�J@�`B@�9X@��m@�S�@�ȴ@�@�Ĝ@�(�@���@�;d@��!@��@��#@�O�@��@�Ĝ@��@�Z@�j@�Z@�A�@�C�@�
=@��!@�M�@�{@���@�G�@���@���@�Z@�t�@�"�@��y@���@�=q@��@��-@�p�@�`B@�G�@�%@��`@��@�bN@�ƨ@�dZ@���@��+@�V@�$�@�@��T@��-@�hs@���@�1@���@�C�@�
=@���@�~�@�{@��@��^@�G�@�V@���@��/@�z�@� �@�b@�1@��;@��P@�33@��@���@��+@�$�@��@���@��-@���@��7@�hs@�`B@�7L@���@��@�9X@�(�@�  @��;@���@�|�@�C�@�33@��H@���@�V@�-@�=q@��@��@�hs@�G�@��D@�r�@�bN@�Z@�ƨ@���@�t�@�\)@�S�@�C�@�o@�
=@�@�
=@��y@��@��@���@���@��@��@��T@���@��-@��7@�7L@���@���@��9@��@���@��u@�r�@�I�@��@���@��P@�|�@�S�@�o@���@�n�@�-@�E�@�J@��@�G�@���@��@�/@��@��@�V@��@���@�Ĝ@��u@�j@�I�@��@��;@��w@�t�@�+@�+@�o@�
=@��@��H@���@�5?@�{@��@��-@��-@���@���@��7@�O�@��@�r�@�I�@�9X@���@�|�@�
=@��H@��R@��!@�v�@�{@��T@���@��^@���@�hs@�/@���@��`@���@��@�bN@�b@�P@
=@~ȴ@~��@~@}�T@}?}@|�@|�D@|�@{�
@{t�@{@z~�@z�@y�@y7L@x��@x��@x�@x1'@w�;@wl�@w+@v�y@v��@vV@v@u�@u?}@t��@t��@t��@tI�@s�m@s�@s@rn�@q��@qX@pĜ@pA�@o�w@o;d@n�@nff@n{@m��@m�h@l��@l�@k�m@k�@k"�@k@j�!@jJ@i�^@i��@ix�@i�@h�`@h�@hbN@hQ�@g�w@g;d@f�y@f��@fV@fE�@f5?@f{@e�@e�T@e@e�h@eV@dZ@c��@cƨ@c�@c33@b��@b=q@ax�@a7L@a�@`Ĝ@`��@`r�@`bN@`A�@`1'@`b@_�@_;d@^��@^�+@^V@^$�@]��@]p�@]/@\�@\Z@\�@[��@["�@Z�!@Zn�@Z�@Y��@Yx�@Y&�@X�@X  @W�;@W�;@W�w@W��@W�P@Wl�@W\)@W;d@W
=@Vȴ@Vff@V5?@V$�@U�@U��@Up�@U�@T�@Tz�@S��@Sƨ@S��@S"�@R�@R�!@R��@Rn�@RJ@Q��@Q�@Q�#@Q�^@Q��@Q�7@QX@P�`@P��@P�9@PbN@PA�@O�@O��@OK�@N��@Nȴ@N��@M�T@Mp�@M/@L��@L�D@Lj@L(�@Kƨ@Kt�@K"�@J��@Jn�@J^5@JM�@J-@J�@JJ@I��@I�@I�#@I��@I�^@Ix�@I�@H��@H�@Hr�@HQ�@G�@G�@G+@G�@F�y@Fȴ@F�R@Fff@FV@F5?@F{@E@E�h@Ep�@Ep�@E`B@E?}@D�/@D��@DI�@D1@Ct�@CC�@C@B�!@B~�@B=q@Ahs@A&�@A%@@��@@��@@�u@@A�@?�;@?�w@?+@>�@>��@>ff@>E�@>$�@=�@=��@=��@=p�@=?}@<j@;�m@;ƨ@;�@;t�@;S�@;S�@:�@:n�@9��@9�#@9��@9x�@97L@8Ĝ@8�9@8�u@81'@7�@7��@7;d@6�@6V@5O�@4�/@4I�@3��@3��@3dZ@3S�@3"�@2��@2��@2~�@2=q@2J@2J@1�#@1�^@1�^@1��@1x�@1X@1�@0��@0�`@0��@0 �@0  @0  @/�@/
=@.�y@.ȴ@.V@.E�@.@-��@-�@-?}@-V@,��@,�D@,9X@,9X@,(�@,�@+��@+t�@+C�@*�!@)��@)�^@)��@(��@(�9@(��@(�u@(�u@(�@(�@(bN@( �@(  @'�w@'�@'��@'\)@'�@&��@&�+@&V@&{@%�T@%@%p�@%/@$�/@$�@$z�@$9X@#��@#��@#��@#�@#dZ@#S�@"�@"�\@"n�@"=q@"�@"J@!��@!��@!%@ ��@ �`@ �`@ ��@ ��@ Ĝ@ Ĝ@ Ĝ@ �9@ ��@ r�@ b@�;@��@��@|�@\)@K�@K�@
=@�@�+@ff@5?@{@�@O�@/@�@V@��@�/@�j@��@z�@j@1@��@t�@S�@33@��@M�@�@J@�@�#@�#@��@x�@7L@&�@�@�@�@��@�@Q�@ �@�;@�;@��@�w@l�@�@�y@��@ff@@�-@p�@�/@��@�D@�D@z�@9X@��@�F@�F@��@�@dZ@"�@��@^5@-@�@J@�@�#@�^@��@�7@X@G�@7L@�@��@�`@��@��@bN@A�@  @�;@�P@K�@�@ȴ@�R@��@��@�+@�+@ff@$�@�@��@�-@�h@`B@V@�j@z�@Z@(�@�
@�F@�F@C�@@
��@
�!@
^5@
=q@
-@
J@	�@	�^@	�7@	7L@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�;B	�HB	�NB	�NB	��B
�B
)�B
Q�B
_;B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
m�B
o�B
~�B
�-B
�/B
��B|�BÖB��B��B�/B�B��BB�B �B-B;dBN�BR�BW
BXBZBhsB}�B�DB�\B�PB�B�B�B�B�B{�Bx�B}�B~�B|�Bt�Br�B��B��B�uB��B�Bx�Bs�BjB^5BF�B>wB6FB33B5?B/B!�B�B{B\B1BB��B��B�B�NB��BǮB�RB��B�%B~�By�BhsB_;BG�B:^B,B'�B �B
=B
��B
��B
�yB
�5B
ȴB
��B
��B
�VB
~�B
bNB
W
B
O�B
R�B
VB
9XB
�B
�B

=B
B	�B	�mB	��B	�jB	��B	l�B	W
B	5?B	2-B	�B	�B	�B	�B	DB��B�B�fB��BB�}B�dB�FB�B��B��B��B�bB�+Bq�BjBe`Be`BcTBq�Bu�Bt�By�B�B�B� B~�B}�Bs�BjBe`BgmBe`Bl�B|�B�B�B�PB�\B�\B�\B�VB�\B�VB�PB�DB�7B�%B}�B{�By�Bl�B_;B\)BYBXBVBT�BVBT�BS�BQ�BO�BK�BK�BJ�BH�BH�BI�BO�BJ�BI�BH�BF�BF�BE�BD�BC�BB�B@�B=qB9XB8RB8RB7LB6FB49B0!B)�B'�B%�B%�B&�B&�B'�B'�B&�B&�B&�B'�B'�B)�B,B-B0!B;dB=qB:^B:^B:^B;dBB�BB�BD�BG�BK�BQ�BS�BS�BT�BVBVBW
BXBYB[#B]/B]/BZBXB[#B^5B]/B]/B^5B]/B]/B^5B`BBaHBbNBdZBe`BgmBl�Bm�Bn�Bm�Bo�Bo�Bp�Br�Bs�Bt�Bs�Bv�Bw�Bz�B~�B~�B�B�B�+B�VB�hB��B��B��B��B��B��B��B��B��B��B�!B�-B�3B�^B�dB�}BBÖBŢBȴBɺBɺB��B��B�B�)B�5B�HB�ZB�`B�mB�B�B�B�B��B��B��B	B	%B	1B		7B	DB	JB	PB	\B	hB	�B	�B	#�B	(�B	,B	/B	33B	7LB	9XB	<jB	E�B	I�B	K�B	L�B	P�B	R�B	S�B	T�B	T�B	VB	XB	XB	[#B	^5B	`BB	dZB	e`B	e`B	gmB	jB	l�B	l�B	n�B	o�B	q�B	r�B	s�B	v�B	w�B	y�B	z�B	{�B	|�B	~�B	� B	�B	�B	�B	�B	�1B	�+B	�DB	�=B	�JB	�VB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�9B	�9B	�9B	�?B	�FB	�RB	�XB	�qB	�qB	�wB	�}B	��B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�5B	�5B	�5B	�5B	�;B	�NB	�TB	�ZB	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
PB
PB
VB
VB
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
oB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
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
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
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
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
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
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
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
T�B
T�B
T�B
T�B
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
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
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
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
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
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
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
jB
jB
jB
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
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�B	�B	�B	�B	��B
_B
)�B
Q�B
_B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^B
_B
m]B
oiB
~�B
��B
��B
��B|�B�aBбB͟B��B�|B�B �BYB �B,�B;0BN�BR�BV�BW�BY�Bh>B}�B�B�(B�B��B��B��B��B��B{�Bx�B}�B~�B|�Bt�Br|B�qB�eB�@B�~B��Bx�Bs�BjKB^BFtB>BB6B2�B5B.�B!�BkBFBB�B�B��B��B�QB�B��B�zB�B�_B��B~�By�Bh>B_BGzB:*B+�B'�B vB
	B
��B
��B
�*B
��B
�fB
��B
�YB
�"B
~�B
bB
V�B
O�B
R�B
U�B
9	B
�B
2B

	B
 �B	�hB	�B	�~B	�B	�2B	lWB	V�B	4�B	1�B	pB	WB	jB	]B	
�B��B�oB�2BԯB�AB�.B�0B��B��B��B�qB�?B�B��Bq[Bj0BeBe,BcBq[ButBt�By�B��B��B�B~�B}�Bs�BjKBeBgBeBl=B|�B��B��B�B�B�(B�B�B�B�B�B�B��B��B}�B{�By�Bl=B^�B[�BX�BW�BU�BT�BU�BT�BS�BQ�BO�BKxBKxBJrBHfBH�BIlBO�BJrBIlBHfBFYBFYBESBDMBCGBBAB@4B="B9	B8B8B6�B5�B3�B/�B)�B'�B%�B%�B&�B&�B'�B'�B&�B&�B&�B'�B'�B)�B+�B,�B/�B;B="B:B:B:B;BBABBABDMBG_BKxBQ�BS�BS�BT�BU�BU�BV�BW�BX�BZ�B\�B\�BY�BW�BZ�B]�B\�B\�B]�B\�B\�B]�B_�B`�Ba�BdBeBgBl=Bm]BnIBm)BoOBoOBpUBraBshBtnBsMBvzBw�Bz�B~�B~�B��B��B��B�B�B�2B�9B�7B�pB�\B�|B��B��B��B��B��B��B��B�B�B�.B�AB�-B�9B�fB�lB�lBΊBөB��B��B��B��B�B�B�B�0B�IB�[B�hB�`B��B��B	 �B	�B	�B	�B	
�B	�B	B	B	B	?B	pB	#�B	(�B	+�B	.�B	2�B	6�B	8�B	<B	ESB	IlB	KxB	L~B	P�B	R�B	S�B	T�B	T�B	U�B	W�B	W�B	Z�B	]�B	_�B	dB	eB	eB	gB	j0B	l=B	l=B	nIB	oOB	q[B	raB	shB	vzB	w�B	yrB	z�B	{B	|�B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�2B	�?B	�EB	�KB	�WB	�]B	�CB	�IB	�jB	�jB	�jB	�OB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�"B	�"B	�(B	�.B	�4B	�'B	�GB	�MB	�MB	�MB	�SB	�_B	�rB	�~B	̈́B	ΊB	ϑB	ϑB	ЗB	ЗB	ЗB	�}B	ЗB	ѝB	ѝB	ѝB	өB	յB	ּB	ּB	רB	רB	��B	خB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�$B	�*B	�B	�0B	�=B	�CB	�/B	�/B	�/B	�/B	�5B	�UB	�UB	�UB	�[B	�[B	�aB	�aB	�hB	�nB	�nB	�nB	�tB	�ZB	�zB	��B	��B	�lB	��B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B

�B

�B

�B
�B
B
B
B
B
B
�B
B
�B
B
B
B
B
B
B
 B
&B
&B
B
&B
&B
,B
,B
,B
B
B
2B
9B
9B
9B
?B
?B
EB
+B
KB
KB
KB
7B
QB
QB
QB
QB
7B
WB
WB
CB
]B
CB
CB
dB
dB
dB
dB
OB
jB
jB
VB
pB
 \B
 vB
 vB
!|B
!|B
!|B
"�B
#�B
#�B
#�B
#�B
#�B
#nB
#�B
#�B
$�B
$�B
$tB
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
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
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
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
-�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
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
3�B
4�B
4�B
4�B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
6�B
8B
8B
8B
8B
8B
9	B
9	B
8�B
:B
:B
:B
:B
:B
;B
;B
:�B
:�B
:�B
:�B
<B
<B
<B
<B
<B
<B
<B
="B
>(B
?.B
?.B
?.B
?.B
@4B
@4B
@4B
@4B
@4B
A;B
A;B
A B
A;B
BAB
CGB
CGB
C-B
CGB
DMB
DMB
DMB
DMB
DMB
DMB
DMB
ESB
ESB
ESB
ESB
E9B
ESB
E9B
ESB
ESB
ESB
ESB
E9B
FYB
FYB
FYB
FYB
FYB
G_B
FYB
FYB
G_B
G_B
G_B
G_B
GEB
G_B
G_B
HfB
HKB
HfB
HfB
HfB
HfB
IlB
IlB
IlB
JrB
JrB
KxB
K^B
LdB
L~B
L~B
L~B
L~B
L~B
L~B
L~B
LdB
L~B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
NpB
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
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
T�B
T�B
T�B
T�B
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
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
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
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
[�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
bB
a�B
a�B
a�B
cB
cB
b�B
cB
cB
dB
c�B
dB
c�B
dB
dB
c�B
dB
eB
eB
eB
eB
eB
eB
eB
fB
fB
fB
e�B
gB
gB
gB
gB
gB
gB
g8B
gB
h$B
h
B
h$B
h$B
h
B
h$B
i*B
iB
i*B
i*B
jB
j0B
jKB
j0B
k6B
k6B
k6B
k6B
l=B
l=B
l=B
l=B
l"B
l=B
l=B
mCB
mC111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.62(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903270036302019032700363020190327003630201903280033372019032800333720190328003337JA  ARFMdecpA19c                                                                20190322033641  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190321183657  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190321183659  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190321183659  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190321183700  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190321183700  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190321183700  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190321183700  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190321183700  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190321183700                      G�O�G�O�G�O�                JA  ARUP                                                                        20190321185618                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190322153447  CV  JULD            G�O�G�O�FŃ,                JM  ARCAJMQC2.0                                                                 20190326153630  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190326153630  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190327153337  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                