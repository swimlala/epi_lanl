CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-09T09:36:42Z creation;2018-11-09T09:36:45Z conversion to V3.1;2019-12-23T06:12:17Z update;     
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݠ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181109093642  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               fA   JA  I2_0675_102                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؏S�� 1   @؏S�r @6��A���cR4�K1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ D�|�DǼ�D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ Dڼ�D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@��
A	�A)�AHQ�Ai�A���A���A���A���A���A���A���A���Bz�B
z�Bz�Bz�B"z�B*z�B2z�B:z�BBz�BJz�BRz�BZz�Bbz�Bjz�Brz�Bzz�B�=qB�=qB�=qB�p�B�=qB�
=B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�
=B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�\)C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D�HD'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ�HDK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm�Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt��Du'�Du��Dv'�Dv��Dw'�Dw��Dx'�Dx��Dy'�Dy��Dz'�Dz��D{'�D{��D|'�D|��D}'�D}��D~'�D~��D'�D��D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D�D���D��D�S�DÓ�D���D��D�S�Dē�D���D��D�S�Dœ�D���D��D�S�DƓ�D���D��D�S�Dǐ�D�ФD��D�S�Dȓ�D���D��D�S�Dɓ�D���D��D�S�Dʓ�D���D��D�S�D˓�D���D��D�S�D̓�D���D��D�S�D͓�D���D��D�S�DΓ�D���D��D�S�Dϓ�D���D��D�S�DГ�D���D��D�S�Dѓ�D���D��D�S�Dғ�D���D��D�S�Dӓ�D���D��D�S�Dԓ�D���D��D�S�DՓ�D���D��D�S�D֓�D���D��D�S�Dד�D���D��D�S�Dؓ�D���D��D�S�Dٓ�D���D��D�S�Dړ�D�ФD��D�S�Dۓ�D���D��D�S�Dܓ�D���D��D�S�Dݓ�D���D��D�S�Dޓ�D���D��D�S�Dߓ�D���D��D�S�D���D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�W
D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A�ƨA�S�A���Aϲ-A���A���A���A�ƨAϬAϟ�Aϕ�AύPAχ+A�|�A�p�A�jA�ffA�dZA�ffA�hsA�jA�hsA�hsA�ffA�dZA�ffA�ffA�hsA�hsA�ffA�O�A΋DA��`A�A��/A�VA�^5A��RA�A�A�XA�~�A�\)A�%A��9A���A�/A���A��A��jA��yA�\)A��A��A��A��7A�`BA� �A���A��A�~�A���A���A� �A���A�-A�p�A��-A�7LA�XA�A���A�G�A��A�9XA��jA���A���A��A��A�ȴA�t�A�G�A�bNA�
=A��A��A�ZA��7A�n�A�Q�A��A�M�A���A��A��A��yA�~�A��A~v�A|��A|bAzn�Ay�Ay%Av�HAt�AtZAs�
Arr�Apr�Am��Ak��Ah��AgAd��AaG�A_�-A]�
A]S�A\��A\�+A[hsAZjAY��AYC�AX9XAVr�AUoAS�AP�HAO7LAMoAKXAI��AHZAGK�AF-AE?}AD�uAC�^A@z�A>��A<�RA<5?A;|�A;A9�wA7C�A6{A4�`A4VA4-A3��A2�A2^5A1��A0��A.A,�/A,=qA+\)A)�7A)VA(�A(ffA'�A%�-A$^5A"��A"r�A!�7A 9XA&�A�RA�A��Ar�AA��A�+A�;A��A��A �A�uAK�A��A�PAS�A�A�A�/A�A�AK�A
ffA	�;A	l�A��AVA��A+AhsAffA�`A�RA�AI�A�FA Ĝ@�+@���@�=q@�x�@��@���@��u@���@��@�ƨ@�33@�R@�9@�K�@�@��@�bN@�@�+@�E�@���@�z�@��@���@�@�?}@߅@݁@�1@ڏ\@أ�@�33@ՙ�@�G�@�r�@�"�@�{@�?}@���@�9X@Η�@�Q�@�\)@ʏ\@��@ɡ�@�?}@���@�Ĝ@ț�@�A�@�o@�/@Ý�@�@�=q@��#@���@��@���@��;@���@�V@�G�@�Q�@���@�ƨ@�|�@�"�@�ȴ@�-@�7L@��m@���@�
=@�=q@��7@��@���@��@���@�I�@�  @�S�@��H@���@�ff@���@�X@��D@�1'@��@�l�@�33@�"�@��@��H@�=q@�{@��-@�G�@��9@�Q�@�ƨ@�33@�M�@��@��#@�@��^@�hs@���@�A�@�b@���@���@�t�@��y@�ff@�E�@�5?@��@��@�@���@��7@��@�Ĝ@���@��@�r�@�Q�@�b@���@�;d@�"�@�o@�@��H@���@�o@�"�@�33@��@�ȴ@���@��@�G�@��@���@��D@���@��@��P@��P@���@���@��P@�l�@���@�n�@��@���@�hs@���@��@�1'@��
@���@�S�@�33@�~�@�@�@���@�hs@�G�@�/@��@���@���@�j@���@�
=@��y@��@��+@��R@��!@�$�@���@�@�@���@�&�@���@�/@�/@�G�@�7L@�V@�bN@�A�@�(�@�I�@�r�@�j@�A�@�  @��;@���@�C�@���@�V@�$�@��#@��h@�O�@��#@��7@��`@��@��D@�r�@�I�@�b@��@�l�@�+@���@�v�@�ff@���@���@��7@�O�@�&�@��@��`@���@��j@��@���@�z�@�r�@�A�@�(�@� �@��@��@�(�@�(�@�b@���@��;@��@��@�\)@�\)@�dZ@�
=@�v�@�$�@�J@�@��@��-@�hs@�&�@��@��j@���@��@�I�@��m@���@�\)@�K�@�+@��H@�ȴ@���@���@��+@�E�@���@��7@�x�@�X@���@��@�bN@�(�@�1@��@|�@K�@~�@~V@~$�@}�-@}?}@|��@|(�@{dZ@{@z��@z�@y�7@y&�@x�`@x��@xĜ@x1'@w��@w;d@v��@vV@vV@v5?@u�T@uO�@u/@t�@tZ@t�@s��@sC�@r��@r^5@rJ@q��@q��@q�7@qhs@qG�@q�@pbN@o�@o+@n�R@n@m?}@mV@l�/@l�D@lZ@lI�@l(�@l1@k��@j�!@jJ@iX@i�@h�9@h �@g�;@g�@f�+@f{@f@e��@d��@c�m@c��@cdZ@cC�@cC�@b��@b~�@bn�@bM�@b-@bJ@a��@a�7@ahs@aG�@a&�@`Ĝ@`r�@_�;@_�P@^ȴ@^v�@]p�@\��@\�@\j@\I�@\(�@[��@[S�@["�@Z��@Zn�@Z�@Y��@Y&�@X�9@Xr�@X  @W�w@W�w@W�@Wl�@V�R@V$�@U�-@U�@T�@Tj@T(�@S�m@S��@S�@SS�@So@R�\@Rn�@RM�@RJ@Q��@QG�@P��@P�u@Pb@N��@NV@N{@M�-@M?}@L�j@LZ@L�@K�
@K��@KS�@J�@J~�@I��@I�@I�#@I��@I��@Ix�@IX@H��@Hr�@H �@G�w@G|�@Gl�@G;d@G+@G�@F�R@F��@Fv�@FV@F@E�-@E`B@D��@DI�@C��@CdZ@Co@B��@B��@B�!@B�!@B�\@Bn�@B-@A��@AX@AG�@A�@@�9@@A�@?�@?�@?\)@?�@>��@>�@>�+@>5?@=�@=@=��@<�@<�D@<(�@;��@;�@;dZ@;S�@;33@;@:�!@:=q@9��@9��@9�7@9hs@97L@9%@8�`@8��@8�`@8��@8r�@7�;@7|�@7+@7
=@6��@6�@6E�@5��@5�@5O�@5V@4��@4�@4�j@4�j@4j@49X@3�
@3ƨ@3��@3dZ@3C�@2�H@2��@2��@2��@2��@1�#@1G�@1�@1%@0��@0Ĝ@0Ĝ@0��@0r�@0A�@01'@/��@/�@/��@/��@/\)@/;d@/�@.��@.�y@.�@.�R@.ff@.{@-@-�@-?}@,��@,��@,��@,j@,(�@+�
@+ƨ@+�F@+�@+33@+@*�H@*�!@*n�@*=q@*=q@*=q@*-@*J@)�@)��@)hs@)G�@)7L@(�`@(��@( �@(  @'�@'��@'�w@'�P@'l�@'K�@&�y@&��@&v�@%�T@%�h@%O�@$��@$�@$Z@#��@#�
@#�F@#�@#S�@"��@"-@!�#@!�^@!x�@!X@ ��@ r�@   @�@|�@l�@�@
=@��@�y@ȴ@�R@v�@5?@@@@@�T@@�h@`B@?}@�@��@��@j@(�@1@�
@t�@C�@33@�@�\@=q@�@J@��@G�@��@Q�@�@�;@�;@�;@�;@�;@�@|�@;d@
=@ȴ@�R@v�@$�@�T@�h@�@�@O�@/@��@�@�D@Z@�@��@dZ@o@��@�\@^5@J@��@�7@7L@�@��@Ĝ@�9@��@��@�u@Q�@A�@ �@b@b@�@��@\)@+@�y@��@��@V@{@�T@��@@�h@O�@V@�@�/@��@�@�D@j@Z@1@�
@�
@ƨ@S�@33@"�@"�@@
�@
��@
~�@
M�@
-@	�@	��@	G�@	�@�`@r�@1'@A�@  @�;@�;@�;@��@�@|�@l�@\)@K�@�@��@ȴ@�+@V@{@@@�@@�-@�h@�h@�h@�@?}@/@V@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A�ƨA�S�A���Aϲ-A���A���A���A�ƨAϬAϟ�Aϕ�AύPAχ+A�|�A�p�A�jA�ffA�dZA�ffA�hsA�jA�hsA�hsA�ffA�dZA�ffA�ffA�hsA�hsA�ffA�O�A΋DA��`A�A��/A�VA�^5A��RA�A�A�XA�~�A�\)A�%A��9A���A�/A���A��A��jA��yA�\)A��A��A��A��7A�`BA� �A���A��A�~�A���A���A� �A���A�-A�p�A��-A�7LA�XA�A���A�G�A��A�9XA��jA���A���A��A��A�ȴA�t�A�G�A�bNA�
=A��A��A�ZA��7A�n�A�Q�A��A�M�A���A��A��A��yA�~�A��A~v�A|��A|bAzn�Ay�Ay%Av�HAt�AtZAs�
Arr�Apr�Am��Ak��Ah��AgAd��AaG�A_�-A]�
A]S�A\��A\�+A[hsAZjAY��AYC�AX9XAVr�AUoAS�AP�HAO7LAMoAKXAI��AHZAGK�AF-AE?}AD�uAC�^A@z�A>��A<�RA<5?A;|�A;A9�wA7C�A6{A4�`A4VA4-A3��A2�A2^5A1��A0��A.A,�/A,=qA+\)A)�7A)VA(�A(ffA'�A%�-A$^5A"��A"r�A!�7A 9XA&�A�RA�A��Ar�AA��A�+A�;A��A��A �A�uAK�A��A�PAS�A�A�A�/A�A�AK�A
ffA	�;A	l�A��AVA��A+AhsAffA�`A�RA�AI�A�FA Ĝ@�+@���@�=q@�x�@��@���@��u@���@��@�ƨ@�33@�R@�9@�K�@�@��@�bN@�@�+@�E�@���@�z�@��@���@�@�?}@߅@݁@�1@ڏ\@أ�@�33@ՙ�@�G�@�r�@�"�@�{@�?}@���@�9X@Η�@�Q�@�\)@ʏ\@��@ɡ�@�?}@���@�Ĝ@ț�@�A�@�o@�/@Ý�@�@�=q@��#@���@��@���@��;@���@�V@�G�@�Q�@���@�ƨ@�|�@�"�@�ȴ@�-@�7L@��m@���@�
=@�=q@��7@��@���@��@���@�I�@�  @�S�@��H@���@�ff@���@�X@��D@�1'@��@�l�@�33@�"�@��@��H@�=q@�{@��-@�G�@��9@�Q�@�ƨ@�33@�M�@��@��#@�@��^@�hs@���@�A�@�b@���@���@�t�@��y@�ff@�E�@�5?@��@��@�@���@��7@��@�Ĝ@���@��@�r�@�Q�@�b@���@�;d@�"�@�o@�@��H@���@�o@�"�@�33@��@�ȴ@���@��@�G�@��@���@��D@���@��@��P@��P@���@���@��P@�l�@���@�n�@��@���@�hs@���@��@�1'@��
@���@�S�@�33@�~�@�@�@���@�hs@�G�@�/@��@���@���@�j@���@�
=@��y@��@��+@��R@��!@�$�@���@�@�@���@�&�@���@�/@�/@�G�@�7L@�V@�bN@�A�@�(�@�I�@�r�@�j@�A�@�  @��;@���@�C�@���@�V@�$�@��#@��h@�O�@��#@��7@��`@��@��D@�r�@�I�@�b@��@�l�@�+@���@�v�@�ff@���@���@��7@�O�@�&�@��@��`@���@��j@��@���@�z�@�r�@�A�@�(�@� �@��@��@�(�@�(�@�b@���@��;@��@��@�\)@�\)@�dZ@�
=@�v�@�$�@�J@�@��@��-@�hs@�&�@��@��j@���@��@�I�@��m@���@�\)@�K�@�+@��H@�ȴ@���@���@��+@�E�@���@��7@�x�@�X@���@��@�bN@�(�@�1@��@|�@K�@~�@~V@~$�@}�-@}?}@|��@|(�@{dZ@{@z��@z�@y�7@y&�@x�`@x��@xĜ@x1'@w��@w;d@v��@vV@vV@v5?@u�T@uO�@u/@t�@tZ@t�@s��@sC�@r��@r^5@rJ@q��@q��@q�7@qhs@qG�@q�@pbN@o�@o+@n�R@n@m?}@mV@l�/@l�D@lZ@lI�@l(�@l1@k��@j�!@jJ@iX@i�@h�9@h �@g�;@g�@f�+@f{@f@e��@d��@c�m@c��@cdZ@cC�@cC�@b��@b~�@bn�@bM�@b-@bJ@a��@a�7@ahs@aG�@a&�@`Ĝ@`r�@_�;@_�P@^ȴ@^v�@]p�@\��@\�@\j@\I�@\(�@[��@[S�@["�@Z��@Zn�@Z�@Y��@Y&�@X�9@Xr�@X  @W�w@W�w@W�@Wl�@V�R@V$�@U�-@U�@T�@Tj@T(�@S�m@S��@S�@SS�@So@R�\@Rn�@RM�@RJ@Q��@QG�@P��@P�u@Pb@N��@NV@N{@M�-@M?}@L�j@LZ@L�@K�
@K��@KS�@J�@J~�@I��@I�@I�#@I��@I��@Ix�@IX@H��@Hr�@H �@G�w@G|�@Gl�@G;d@G+@G�@F�R@F��@Fv�@FV@F@E�-@E`B@D��@DI�@C��@CdZ@Co@B��@B��@B�!@B�!@B�\@Bn�@B-@A��@AX@AG�@A�@@�9@@A�@?�@?�@?\)@?�@>��@>�@>�+@>5?@=�@=@=��@<�@<�D@<(�@;��@;�@;dZ@;S�@;33@;@:�!@:=q@9��@9��@9�7@9hs@97L@9%@8�`@8��@8�`@8��@8r�@7�;@7|�@7+@7
=@6��@6�@6E�@5��@5�@5O�@5V@4��@4�@4�j@4�j@4j@49X@3�
@3ƨ@3��@3dZ@3C�@2�H@2��@2��@2��@2��@1�#@1G�@1�@1%@0��@0Ĝ@0Ĝ@0��@0r�@0A�@01'@/��@/�@/��@/��@/\)@/;d@/�@.��@.�y@.�@.�R@.ff@.{@-@-�@-?}@,��@,��@,��@,j@,(�@+�
@+ƨ@+�F@+�@+33@+@*�H@*�!@*n�@*=q@*=q@*=q@*-@*J@)�@)��@)hs@)G�@)7L@(�`@(��@( �@(  @'�@'��@'�w@'�P@'l�@'K�@&�y@&��@&v�@%�T@%�h@%O�@$��@$�@$Z@#��@#�
@#�F@#�@#S�@"��@"-@!�#@!�^@!x�@!X@ ��@ r�@   @�@|�@l�@�@
=@��@�y@ȴ@�R@v�@5?@@@@@�T@@�h@`B@?}@�@��@��@j@(�@1@�
@t�@C�@33@�@�\@=q@�@J@��@G�@��@Q�@�@�;@�;@�;@�;@�;@�@|�@;d@
=@ȴ@�R@v�@$�@�T@�h@�@�@O�@/@��@�@�D@Z@�@��@dZ@o@��@�\@^5@J@��@�7@7L@�@��@Ĝ@�9@��@��@�u@Q�@A�@ �@b@b@�@��@\)@+@�y@��@��@V@{@�T@��@@�h@O�@V@�@�/@��@�@�D@j@Z@1@�
@�
@ƨ@S�@33@"�@"�@@
�@
��@
~�@
M�@
-@	�@	��@	G�@	�@�`@r�@1'@A�@  @�;@�;@�;@��@�@|�@l�@\)@K�@�@��@ȴ@�+@V@{@@@�@@�-@�h@�h@�h@�@?}@/@V@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�ZB�fB�B��B+BPB�B>wBQ�BS�BR�BQ�BQ�BQ�BQ�BQ�BP�BP�BP�BP�BP�BP�BR�BR�BS�BR�BR�BS�BT�BT�BT�BT�BS�B`BBt�B~�B��B��B�RB�XB�RB�?B�3B�'B�B��B��B��B��B��B��B�{B�PB�7B� Bw�Bv�B}�Bz�B�B�B�7B�+B�B}�Bz�Bu�Bp�B_;BT�BR�BA�B1'BJB��B�fB�B��BǮB�B��B��B��B�BI�B+B �B	7B
��B
�B
ɺB
�#B
ĜB
�B
��B
��B
��B
�PB
�DB
y�B
_;B
J�B
J�B
G�B
@�B
=qB
:^B
$�B
�B
�B
{B
B	�B	�BB	ƨB	�^B	��B	�B	{�B	s�B	m�B	jB	ffB	aHB	XB	S�B	N�B	K�B	>wB	6FB	,B	"�B	{B	DB	  B��B�B�yB�ZB�HB�)B�BĜB�LB��B��B��B��B�bB�+B�1B�+B�+B�=B�JB�VB�bB�JB�7By�BjBgmBgmBffBgmBhsBiyBhsBjBl�Bl�Bl�BiyBgmBcTB]/BW
BT�BP�BM�BJ�BI�BJ�BE�BE�BD�BC�B?}B<jB;dB9XB8RB8RB6FB6FB49B33B33B1'B0!B/B.B.B,B/B,B)�B(�B(�B(�B'�B(�B&�B%�B$�B"�B"�B"�B"�B#�B%�B$�B$�B$�B"�B"�B!�B&�B'�B'�B(�B(�B(�B)�B)�B(�B(�B(�B+B,B-B.B0!B0!B0!B/B/B0!B0!B0!B/B1'B49B7LB8RB:^B:^B<jB=qB=qB>wB>wB?}BC�BF�BH�BJ�BJ�BJ�BK�BM�BN�BS�BYBZB[#B^5B^5B_;B`BBaHBbNBdZBe`BiyBhsBl�Bm�Bo�Br�Bw�Bw�B{�B~�B� B�B�%B�+B�1B�DB�PB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�FB�LB�RB�RB�^B�qBBĜBȴB��B��B��B�B�#B�#B�)B�5B�BB�BB�NB�sB�B�B��B��B��B	B	+B		7B		7B	DB	PB	bB	�B	�B	�B	�B	!�B	'�B	)�B	,B	1'B	6FB	7LB	9XB	:^B	;dB	;dB	<jB	=qB	>wB	?}B	?}B	@�B	@�B	@�B	A�B	C�B	E�B	G�B	J�B	L�B	M�B	P�B	Q�B	VB	YB	ZB	]/B	aHB	cTB	dZB	ffB	hsB	iyB	l�B	m�B	p�B	r�B	t�B	t�B	v�B	w�B	y�B	z�B	{�B	{�B	}�B	�B	�B	�=B	�PB	�\B	�hB	�oB	�hB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�-B	�9B	�9B	�?B	�RB	�^B	�XB	�XB	�^B	�dB	�qB	�wB	�qB	�}B	ÖB	ĜB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�#B	�)B	�)B	�5B	�;B	�;B	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
JB
PB
VB
VB
VB
\B
\B
\B
hB
hB
hB
hB
hB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
,B
,B
,B
-B
-B
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
1'B
1'B
1'B
1'B
2-B
2-B
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
7LB
7LB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
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
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
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
E�B
F�B
F�B
F�B
F�B
F�B
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
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
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
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
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
M�B
N�B
N�B
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
S�B
S�B
S�B
S�B
S�B
T�B
T�B
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
W
B
XB
XB
XB
XB
XB
XB
XB
YB
YB
ZB
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
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
_;B
_;B
_;B
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
e`B
e`B
e`B
e`B
ffB
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
jB
jB
k�B
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
l�B
l�B
l�B
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
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�&B�2B�oB��B�BBxB>BBQ�BS�BR�BQ�BQ�BQ�BQ�BQ�BP�BP�BP�BP�BP�BP�BR�BR�BS�BR�BR�BS�BT�BT�BT�BT�BS�B`Bt�B~�B�_B��B�B�$B�B�B��B��B��B��B��B�kB�eB�YB�MB�FB�B�B�Bw�Bv�B}�Bz�B��B��B�B��B��B}�Bz�ButBpUB^�BT�BR�BAUB0�BB��B�2B��B��B�zB��B��B�jB��B��BIlB*�B �B	B
�tB
��B
�lB
��B
�gB
��B
��B
��B
�qB
�B
�B
y�B
_B
J�B
JrB
GzB
@4B
=<B
:*B
$�B
dB
WB
,B
�B	�cB	�B	�tB	�*B	��B	��B	{�B	s�B	mCB	jKB	f2B	`�B	W�B	S�B	N�B	K�B	>(B	6B	+�B	"�B	FB	
�B��B�nB�WB�*B�&B��B��B��B�gB��B��B��B�~B�?B�B��B��B��B��B�	B��B�B�.B��B��By�BjKBgBgBfBgBh$Bi*Bh$Bj0Bl=Bl=Bl=Bi*BgBcB\�BV�BT�BP�BM�BJrBI�BJrBEmBESBDMBCGB?.B<B;B9	B8B8B5�B5�B3�B2�B2�B0�B/�B.�B-�B-�B+�B.�B+�B)�B(�B(�B(�B'�B(�B&�B%�B$�B"�B"�B"�B"�B#�B%�B$�B$�B$�B"�B"�B!|B&�B'�B'�B(�B(�B(�B)�B)�B(�B(�B(�B*�B+�B,�B-�B/�B/�B/�B.�B.�B/�B/�B/�B.�B0�B3�B6�B8B:B:B<B="B="B>(B>(B?.BCGBFYBHfBJrBJrBJ�BKxBM�BN�BS�BX�BY�BZ�B]�B]�B^�B_�B`�Ba�BdBeBi*Bh$Bl=BmCBoOBraBwfBw�B{�B~�B�B��B��B��B��B��B�B� B� B�2B�B�$B�KB�KB�QB�dB�jB�vB��B��B��B��B��B��B��B��B��B�B�B�"B�AB�MB�fB�XB�~BңB��B��B��B��B��B��B��B��B�$B�CB�UB�nB�zB��B	�B	�B	�B	�B	
�B	�B	B	B	?B	KB	dB	!|B	'�B	)�B	+�B	0�B	5�B	6�B	9	B	:B	:�B	;B	<B	="B	>(B	?.B	?.B	@4B	@4B	@B	A;B	C-B	ESB	G_B	JrB	L~B	M�B	P�B	Q�B	U�B	X�B	Y�B	\�B	`�B	cB	dB	fB	h$B	i*B	l=B	m)B	pUB	rGB	tnB	tTB	vzB	w�B	y�B	z�B	{�B	{�B	}�B	��B	��B	��B	��B	�B	�B	� B	�B	�B	�B	�&B	�$B	�EB	�EB	�EB	�EB	�KB	�QB	�KB	�EB	�KB	�QB	�dB	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�	B	�B	�B	�"B	�(B	�"B	�B	�GB	�MB	�SB	�SB	�_B	�lB	�xB	�~B	�~B	�jB	̈́B	ϑB	ѝB	ԯB	յB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�
B	�
B	�$B	�$B	�*B	�*B	�B	�6B	�"B	�CB	�IB	�5B	�UB	�[B	�[B	�[B	�[B	�aB	�aB	�hB	�hB	�nB	�nB	�nB	�`B	�fB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B

�B

�B

�B
�B
�B
�B
�B
B
�B
B
B
B
B
�B
B
B
B
 B
 B
&B
,B
,B
,B
B
,B
B
2B
2B
2B
2B
2B
B
9B
B
9B
9B
?B
EB
1B
KB
QB
WB
dB
dB
IB
jB
IB
jB
jB
jB
VB
pB
pB
 vB
 vB
 vB
!bB
!|B
"�B
"�B
"hB
"�B
"hB
#nB
$�B
$�B
$�B
%zB
%zB
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+�B
+�B
+�B
,�B
,�B
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
0�B
0�B
0�B
0�B
1�B
1�B
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
6�B
6�B
8B
8B
8B
9	B
9	B
:B
:B
:B
;B
;B
;B
:�B
;B
<B
<B
<B
<B
="B
="B
="B
>(B
>(B
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
@4B
@4B
@B
A;B
A B
A;B
A;B
A;B
BAB
BAB
B'B
BAB
BAB
CGB
CGB
CGB
CGB
C-B
CGB
C-B
DMB
DMB
ESB
ESB
FYB
F?B
FYB
F?B
FYB
G_B
G_B
G_B
GEB
GEB
G_B
HKB
HKB
HfB
HfB
HfB
HfB
HfB
HfB
HfB
HfB
HfB
IlB
IlB
IlB
IlB
IlB
IlB
IRB
IlB
IRB
IlB
IlB
JXB
JrB
IlB
IlB
IlB
JrB
JrB
JrB
JrB
JrB
JrB
JrB
JrB
K^B
K^B
KxB
KxB
KxB
KxB
K^B
KxB
KxB
KxB
KxB
L~B
L~B
L~B
L~B
L~B
L~B
MjB
M�B
M�B
MjB
M�B
M�B
M�B
N�B
NpB
O�B
O�B
P�B
P�B
P}B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
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
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
Y�B
Z�B
Z�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
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
^�B
^�B
^�B
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
a�B
a�B
cB
b�B
b�B
cB
cB
dB
c�B
c�B
dB
eB
eB
eB
eB
f2B
e�B
fB
fB
fB
gB
gB
gB
gB
gB
gB
gB
gB
gB
gB
h>B
h$B
h$B
h$B
h$B
h
B
i*B
i*B
i*B
i*B
j0B
j0B
j0B
j0B
j0B
j0B
k6B
k6B
k6B
k6B
k6B
k6B
kB
k6B
l=B
l=B
l=B
l=B
lWB
l=B
l=B
l=B
l=B
l"B
mCB
mCB
mCB
mCB
nIB
nIB
nIB
nIB
nIB
nIB
o5B
oOB
o5B
pUB
poB
pUB
pUB
pUB
pUB
pUB
pUB
pUB
pUB
pUB
pUB
pUB
q[B
qAB
q[B
q[B
q[B
qAB
r|B
raB
r|B
raB
raB
raB
shB
shB
shG�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.62(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811150044022018111500440220181115004402201811160035022018111600350220181116003502JA  ARFMdecpA19c                                                                20181109183635  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181109093642  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181109093644  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181109093644  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181109093645  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181109093645  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181109093645  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181109093645  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181109093645  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181109093645                      G�O�G�O�G�O�                JA  ARUP                                                                        20181109095621                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181109153631  CV  JULD            G�O�G�O�F�z�                JM  ARSQJMQC2.0                                                                 20181113000000  CF  PSAL_ADJUSTED_QCD�p D�p G�O�                JM  ARCAJMQC2.0                                                                 20181114154402  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181114154402  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181115153502  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                