CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-02T00:36:39Z creation;2018-10-02T00:36:45Z conversion to V3.1;2019-12-23T06:14:10Z update;     
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181002003639  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ]A   JA  I2_0675_093                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؅�˩��1   @؅�}'Ҁ@7���ݘ�cUA��s1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  @���A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DC��DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D��3D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�<�D΀ D�� D�3D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D��3D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @^{@�=q@�=qA�A)�AI�Ai�A�\)A��\A��\A��\Aď\Aԏ\A�\A�\BG�B
G�BG�BG�B"G�B*G�B2G�B:G�BBG�BJG�BRG�BZG�BbG�BjG�Br�By�HB�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�U�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�U�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�D ${D �{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D	${D	�{D
${D
�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D ${D �{D!${D!�{D"${D"�{D#${D#�D$${D$�{D%${D%�{D&${D&�{D'${D'�{D(${D(�{D)${D)�{D*${D*�{D+${D+�{D,${D,�{D-${D-�{D.${D.�{D/${D/�{D0${D0�{D1${D1�{D2${D2�{D3${D3�{D4${D4�{D5${D5�{D6${D6�{D7${D7�{D8${D8�{D9${D9�{D:${D:�{D;${D;�{D<${D<�{D=${D=�{D>${D>�{D?${D?�{D@${D@�{DA${DA�{DB${DB�{DC${DC�DDDD�DE${DE�{DF${DF�{DG${DG�{DH${DH�{DI${DI�{DJ${DJ�{DK${DK�{DL${DL�{DM${DM�{DN${DN�{DO${DO�{DP${DP�{DQ${DQ�{DR${DR�{DS${DS�{DT${DT�{DU${DU�{DV${DV�{DW${DW�{DX${DX�{DY${DY�{DZ${DZ�{D[${D[�{D\${D\�{D]${D]�{D^${D^�{D_${D_�{D`${D`�{Da${Da�{Db${Db�{Dc${Dc�{Dd${Dd�{De${De�{Df${Df�{Dg${Dg�{Dh${Dh�{Di${Di�{Dj${Dj�{Dk${Dk�{Dl${Dl�{Dm${Dm�{Dn${Dn�{Do${Do�{Dp${Dp�{Dq${Dq�{Dr${Dr�{Ds${Ds�{Dt${Dt�{Du${Du�{Dv${Dv�{Dw${Dw�{Dx${Dx�{Dy${Dy�{Dz${Dz�{D{${D{�{D|${D|�{D}${D}�{D~${D~�{D${D�{D�=D�R=D��=D��=D�=D�R=D��qD��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��
D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�O
D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��qD��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��qD��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D=D��=D�=D�R=DÒ=D��=D�=D�R=DĒ=D��=D�=D�R=DŒ=D��=D�=D�R=Dƒ=D��=D�=D�R=Dǒ=D��=D�=D�R=DȒ=D��=D�=D�R=Dɒ=D��=D�=D�R=Dʒ=D��=D�=D�R=D˒=D��qD�=D�R=D̒=D��=D�=D�R=D͒=D��=D�=D�O
DΒ=D��=D�qD�R=Dϒ=D��=D�=D�R=DВ=D��=D�=D�R=Dђ=D��=D�=D�R=DҒ=D��=D�=D�R=DӒ=D��qD�=D�R=DԒ=D��=D�=D�R=DՒ=D��=D�=D�R=D֒=D��=D�=D�R=Dג=D��=D�=D�R=Dؒ=D��=D�=D�R=Dْ=D��=D�=D�R=Dڒ=D��=D�=D�R=Dے=D��=D�=D�R=Dܒ=D��=D�=D�R=Dݒ=D��=D�=D�R=Dޒ=D��=D�=D�R=Dߒ=D��=D�=D�R=D��
D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�UqD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7LA��A���A��TA���A���AҺ^A�?}Aћ�A�JAЬAЃA�\)A�;dA�33A��A�1A���A��A��TA��
A�ȴA�AϸRAϮAϥ�AϏ\A�bNA� �A���A��Aư!A�ZA���A�(�A�1A�A��A�n�A��A���A���A���A��A�1A�ƨA�7LA���A��FA��RA�9XA��A���A�$�A�n�A��hA� �A�
=A��jA�  A��wA���A�r�A�O�A�VA���A��A�
=A�33A�x�A��hA��A�1A��!A��A�%A�9XA��;A���A���A�oA���A���A��A��A�(�A��jA�\)A��A�v�A�;dA�"�A���A�(�A�I�A�1A���A�`BA���A�5?A��A��A���A�~�A�1A~��A}l�A|  Ay�;Ax�Au��Au?}At-Aq�AoK�Am�
Al~�Aj�9Ah�Ag�^AfE�Ad��Ac&�Aa�A`=qA_�-A^��A\�A[�AZbAYp�AX�AX��AXbAW��AV�HAV{ATA�ARQ�AQ�-AQ�hAP�AO�AN�AL�9AK�mAKC�AIƨAH�AGp�AF�9AFVAF=qAE�AD��AD$�AC�ABI�AA�7A@�A?"�A>v�A=�wA<��A;/A9�A9S�A9&�A8�9A7"�A5hsA3�A1hsA/%A-�hA,��A,M�A+��A+K�A*�uA*ffA*Q�A*  A)�A(��A'hsA%ƨA#ƨA"Q�A!/A bNA33A�A�!A{AA�`A��AZA��A��AffA�A$�A1'A�/A^5A�mAx�A%A��A�+A�7A��AK�A
�A�HAVAAoA�!A��A+A��A��A��A �yA n�@�ȴ@�G�@�  @�@���@��P@��7@��H@�Z@�R@���@���@�h@��@�u@�K�@�+@��@�M�@�I�@�S�@��#@���@�J@�/@�j@�l�@�$�@��
@�{@ϥ�@�hs@��@̛�@��H@�{@ț�@�ƨ@�t�@Ə\@ř�@�%@�r�@�t�@��@\@�M�@�hs@�j@��m@��@��R@�X@�bN@�o@�V@�O�@��@�t�@���@��@��R@��@��#@�?}@�I�@�|�@�+@���@�=q@�X@�z�@��m@���@�S�@��@�v�@�p�@��@��@���@���@�t�@�o@���@�-@���@�?}@�bN@���@���@�dZ@�=q@���@��#@���@�?}@��@��@�j@�t�@�;d@�
=@�v�@�-@��@��-@�x�@��@��@���@��@���@�j@�Q�@�(�@��m@��@��@��@�Q�@�A�@�bN@�1@��;@��
@��F@���@�C�@�ȴ@���@��+@�^5@�=q@��T@���@�X@�%@�Z@��@��w@��F@�C�@���@�E�@�{@���@��#@���@���@���@�@��^@��-@���@��7@�p�@�X@���@�Ĝ@��@�j@�(�@��m@�ƨ@���@�dZ@�S�@�;d@�@�ȴ@�~�@�$�@���@�J@�{@��^@�`B@�/@�%@�V@��@��@��@�Ĝ@���@�j@�bN@�I�@�  @���@�33@��@�ȴ@���@��\@�^5@��T@�p�@�O�@�/@�&�@�7L@�V@���@��@�Z@� �@��F@�K�@��@���@��+@�ff@�n�@�M�@�E�@�E�@�$�@��T@�X@��@���@�j@�A�@�b@��;@��@���@�S�@�+@�
=@��y@���@�V@�J@��@��^@��h@�X@��@��@�%@���@��D@�Z@�I�@�A�@��@���@�ƨ@���@�33@�@���@�ȴ@���@�n�@�V@��@��^@�`B@�G�@�/@��@�V@���@��@���@�r�@�1'@�ƨ@�S�@�33@�"�@�
=@�@���@�n�@�-@��@�@���@��#@���@��-@�X@��@���@���@�z�@�9X@�;@�P@~��@~�R@~��@~ff@}@}��@}��@}O�@}�@|�@|�D@|z�@|�@{�m@{�
@{�
@{��@{S�@{@z^5@zJ@y��@x��@xĜ@x��@xbN@w\)@w+@w�@v�@v@u�@v@u�@u��@t�j@tI�@t1@s�
@st�@r�\@rJ@q�#@q&�@p�u@p�@p1'@n��@m��@m��@mO�@lZ@k�@j��@j~�@jJ@i��@iG�@hĜ@g�w@g�@fv�@f5?@f{@e�T@e�@eO�@e/@dZ@d(�@c�m@cƨ@c��@cS�@b��@b�!@bn�@bn�@b-@bJ@ax�@`��@`Ĝ@`��@`��@`�@`Q�@` �@_��@_|�@^��@^V@^@]�@]V@\��@\(�@[ƨ@[�@[�@[t�@[dZ@[C�@Z�@Z�!@Y�@Y��@Y��@Y��@Y�7@YX@X��@X  @W�@W��@Wl�@W+@W+@W�@W
=@V�@V��@V��@V{@UV@T�@S�
@SdZ@SS�@S33@S"�@So@S@R��@Rn�@RM�@R=q@RJ@Q�@Q��@Qx�@Q�@P�`@P�@Pb@O\)@N�y@Nv�@Nff@Nff@NE�@M��@MO�@L�@L�j@LI�@K�m@K�F@KdZ@K"�@J�@J�H@J~�@J^5@J=q@JJ@I�#@IX@IG�@I%@H�u@Hr�@Hr�@Hr�@HbN@Hb@G|�@G;d@G;d@F��@F��@F�+@Fff@Fff@F@E/@D�j@D��@C��@C��@C�@CC�@C"�@B��@B~�@B�@A��@@��@@�@?�w@?;d@>�y@>��@>V@=�@=��@=�-@=`B@=?}@=?}@=/@=�@<�/@<�D@<9X@;�m@;ƨ@;S�@;"�@:�!@:J@9X@97L@97L@9&�@9%@8�@8Q�@8 �@8 �@8b@8b@7�@7��@7|�@7\)@7K�@7+@6��@6��@6E�@5@5��@5�@5p�@5?}@5?}@4��@4�/@4�j@4�@4��@4z�@4�@3ƨ@3�F@3��@3�@3S�@3"�@2�@2��@2n�@2M�@2-@1�^@1%@0�u@0�@0�u@0�@0r�@0Q�@0  @/��@/��@/�P@/�P@/l�@/l�@/K�@/�@.�@.�+@.V@.E�@-�@-/@,�/@,�D@,I�@,(�@,�@+�m@+��@+�@+S�@+"�@*�H@*��@*��@*��@*~�@*~�@*^5@*=q@)�#@)��@)��@)G�@(��@(�`@(�`@(��@(��@(�u@'�;@'��@'�w@'�@'�P@'�P@'|�@'\)@'+@&�y@&�+@%�T@%�h@$��@$z�@$I�@#�m@#��@#dZ@#"�@"��@"-@!��@!��@!G�@ ��@ �u@ bN@ A�@ A�@ b@��@�@�P@K�@+@�@��@�+@ff@{@@`B@?}@?}@�@I�@1@��@�@S�@@-@J@�@��@�^@��@�7@hs@%@Q�@  @�;@�;@�@�P@�P@|�@l�@��@V@E�@5?@5?@{@�T@p�@�/@I�@ƨ@o@��@��@n�@=q@=q@�@�@�#@��@�^@��@��@X@�@�@%@��@Ĝ@��@�u@�@bN@bN@Q�@1'@�@�P@|�@;d@+@�@��@ȴ@��@��@��@��@�+@{@�T@�-@��@��@?}@�/@�j@��@Z@(�@��@��@S�@33@"�@"�@o@
�@
��@
�\@
n�@
=q@
-@
�@	�@	�#@	��@	�7@	%@��@Ĝ@��@��@r�@r�@bN@Q�@Q�@b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7LA��A���A��TA���A���AҺ^A�?}Aћ�A�JAЬAЃA�\)A�;dA�33A��A�1A���A��A��TA��
A�ȴA�AϸRAϮAϥ�AϏ\A�bNA� �A���A��Aư!A�ZA���A�(�A�1A�A��A�n�A��A���A���A���A��A�1A�ƨA�7LA���A��FA��RA�9XA��A���A�$�A�n�A��hA� �A�
=A��jA�  A��wA���A�r�A�O�A�VA���A��A�
=A�33A�x�A��hA��A�1A��!A��A�%A�9XA��;A���A���A�oA���A���A��A��A�(�A��jA�\)A��A�v�A�;dA�"�A���A�(�A�I�A�1A���A�`BA���A�5?A��A��A���A�~�A�1A~��A}l�A|  Ay�;Ax�Au��Au?}At-Aq�AoK�Am�
Al~�Aj�9Ah�Ag�^AfE�Ad��Ac&�Aa�A`=qA_�-A^��A\�A[�AZbAYp�AX�AX��AXbAW��AV�HAV{ATA�ARQ�AQ�-AQ�hAP�AO�AN�AL�9AK�mAKC�AIƨAH�AGp�AF�9AFVAF=qAE�AD��AD$�AC�ABI�AA�7A@�A?"�A>v�A=�wA<��A;/A9�A9S�A9&�A8�9A7"�A5hsA3�A1hsA/%A-�hA,��A,M�A+��A+K�A*�uA*ffA*Q�A*  A)�A(��A'hsA%ƨA#ƨA"Q�A!/A bNA33A�A�!A{AA�`A��AZA��A��AffA�A$�A1'A�/A^5A�mAx�A%A��A�+A�7A��AK�A
�A�HAVAAoA�!A��A+A��A��A��A �yA n�@�ȴ@�G�@�  @�@���@��P@��7@��H@�Z@�R@���@���@�h@��@�u@�K�@�+@��@�M�@�I�@�S�@��#@���@�J@�/@�j@�l�@�$�@��
@�{@ϥ�@�hs@��@̛�@��H@�{@ț�@�ƨ@�t�@Ə\@ř�@�%@�r�@�t�@��@\@�M�@�hs@�j@��m@��@��R@�X@�bN@�o@�V@�O�@��@�t�@���@��@��R@��@��#@�?}@�I�@�|�@�+@���@�=q@�X@�z�@��m@���@�S�@��@�v�@�p�@��@��@���@���@�t�@�o@���@�-@���@�?}@�bN@���@���@�dZ@�=q@���@��#@���@�?}@��@��@�j@�t�@�;d@�
=@�v�@�-@��@��-@�x�@��@��@���@��@���@�j@�Q�@�(�@��m@��@��@��@�Q�@�A�@�bN@�1@��;@��
@��F@���@�C�@�ȴ@���@��+@�^5@�=q@��T@���@�X@�%@�Z@��@��w@��F@�C�@���@�E�@�{@���@��#@���@���@���@�@��^@��-@���@��7@�p�@�X@���@�Ĝ@��@�j@�(�@��m@�ƨ@���@�dZ@�S�@�;d@�@�ȴ@�~�@�$�@���@�J@�{@��^@�`B@�/@�%@�V@��@��@��@�Ĝ@���@�j@�bN@�I�@�  @���@�33@��@�ȴ@���@��\@�^5@��T@�p�@�O�@�/@�&�@�7L@�V@���@��@�Z@� �@��F@�K�@��@���@��+@�ff@�n�@�M�@�E�@�E�@�$�@��T@�X@��@���@�j@�A�@�b@��;@��@���@�S�@�+@�
=@��y@���@�V@�J@��@��^@��h@�X@��@��@�%@���@��D@�Z@�I�@�A�@��@���@�ƨ@���@�33@�@���@�ȴ@���@�n�@�V@��@��^@�`B@�G�@�/@��@�V@���@��@���@�r�@�1'@�ƨ@�S�@�33@�"�@�
=@�@���@�n�@�-@��@�@���@��#@���@��-@�X@��@���@���@�z�@�9X@�;@�P@~��@~�R@~��@~ff@}@}��@}��@}O�@}�@|�@|�D@|z�@|�@{�m@{�
@{�
@{��@{S�@{@z^5@zJ@y��@x��@xĜ@x��@xbN@w\)@w+@w�@v�@v@u�@v@u�@u��@t�j@tI�@t1@s�
@st�@r�\@rJ@q�#@q&�@p�u@p�@p1'@n��@m��@m��@mO�@lZ@k�@j��@j~�@jJ@i��@iG�@hĜ@g�w@g�@fv�@f5?@f{@e�T@e�@eO�@e/@dZ@d(�@c�m@cƨ@c��@cS�@b��@b�!@bn�@bn�@b-@bJ@ax�@`��@`Ĝ@`��@`��@`�@`Q�@` �@_��@_|�@^��@^V@^@]�@]V@\��@\(�@[ƨ@[�@[�@[t�@[dZ@[C�@Z�@Z�!@Y�@Y��@Y��@Y��@Y�7@YX@X��@X  @W�@W��@Wl�@W+@W+@W�@W
=@V�@V��@V��@V{@UV@T�@S�
@SdZ@SS�@S33@S"�@So@S@R��@Rn�@RM�@R=q@RJ@Q�@Q��@Qx�@Q�@P�`@P�@Pb@O\)@N�y@Nv�@Nff@Nff@NE�@M��@MO�@L�@L�j@LI�@K�m@K�F@KdZ@K"�@J�@J�H@J~�@J^5@J=q@JJ@I�#@IX@IG�@I%@H�u@Hr�@Hr�@Hr�@HbN@Hb@G|�@G;d@G;d@F��@F��@F�+@Fff@Fff@F@E/@D�j@D��@C��@C��@C�@CC�@C"�@B��@B~�@B�@A��@@��@@�@?�w@?;d@>�y@>��@>V@=�@=��@=�-@=`B@=?}@=?}@=/@=�@<�/@<�D@<9X@;�m@;ƨ@;S�@;"�@:�!@:J@9X@97L@97L@9&�@9%@8�@8Q�@8 �@8 �@8b@8b@7�@7��@7|�@7\)@7K�@7+@6��@6��@6E�@5@5��@5�@5p�@5?}@5?}@4��@4�/@4�j@4�@4��@4z�@4�@3ƨ@3�F@3��@3�@3S�@3"�@2�@2��@2n�@2M�@2-@1�^@1%@0�u@0�@0�u@0�@0r�@0Q�@0  @/��@/��@/�P@/�P@/l�@/l�@/K�@/�@.�@.�+@.V@.E�@-�@-/@,�/@,�D@,I�@,(�@,�@+�m@+��@+�@+S�@+"�@*�H@*��@*��@*��@*~�@*~�@*^5@*=q@)�#@)��@)��@)G�@(��@(�`@(�`@(��@(��@(�u@'�;@'��@'�w@'�@'�P@'�P@'|�@'\)@'+@&�y@&�+@%�T@%�h@$��@$z�@$I�@#�m@#��@#dZ@#"�@"��@"-@!��@!��@!G�@ ��@ �u@ bN@ A�@ A�@ b@��@�@�P@K�@+@�@��@�+@ff@{@@`B@?}@?}@�@I�@1@��@�@S�@@-@J@�@��@�^@��@�7@hs@%@Q�@  @�;@�;@�@�P@�P@|�@l�@��@V@E�@5?@5?@{@�T@p�@�/@I�@ƨ@o@��@��@n�@=q@=q@�@�@�#@��@�^@��@��@X@�@�@%@��@Ĝ@��@�u@�@bN@bN@Q�@1'@�@�P@|�@;d@+@�@��@ȴ@��@��@��@��@�+@{@�T@�-@��@��@?}@�/@�j@��@Z@(�@��@��@S�@33@"�@"�@o@
�@
��@
�\@
n�@
=q@
-@
�@	�@	�#@	��@	�7@	%@��@Ĝ@��@��@r�@r�@bN@Q�@Q�@b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B;dB9XB9XB9XB8RB8RB7LB33B.B(�B&�B'�B'�B&�B)�B'�B'�B&�B&�B&�B&�B'�B'�B'�B'�B(�B,B2-B;dB>wBD�BiyB�B��B��B��B�B�bB�\B�VB�=B�JB�uB�uB��B�oB�hB�VB�1B�bB��B�JB�+B�B�B}�Bt�Br�Bn�B`BB]/BZBQ�BP�BK�BG�B_;BcTBN�B=qB-B+B%�B �B�B  B�B�B�B�NBɺB��B��B�{B�bB�7B�B{�Bu�Bn�Bn�B_;BM�BG�B8RB1'B)�BbB
�B
�)B
ŢB
��B
q�B
B�B
!�B
uB
B	��B	�B	�TB	�
B	�
B	�B	��B	ƨB	�wB	�LB	�?B	�B	��B	��B	�oB	�1B	~�B	u�B	o�B	jB	_;B	VB	L�B	H�B	D�B	A�B	>wB	;dB	6FB	2-B	(�B	"�B	�B	�B	�B	�B	oB	DB	1B	
=B	%B	B��B��B��B��B��B��B�B�B�mB�`B�5B�B��B��B��BÖB�dB�RB�RB�RB�'B��B��B�bB�B� B}�B|�B{�B{�Bz�By�By�Bx�Bw�Bu�Br�Bo�BjBe`BbNBbNB`BB]/BZBW
BVBT�BS�BO�BL�BH�BF�BD�BA�B?}B=qB<jB;dB:^B9XB8RB7LB8RB5?B33B5?B49B2-B2-B33B2-B1'B,B-B)�B'�B$�B#�B"�B!�B �B�B�B�B�B�B�B!�B"�B%�B'�B'�B(�B+B+B+B,B,B,B,B+B,B,B,B,B,B.B.B2-B49B49B49B7LB9XB=qB@�BA�BE�BG�BH�BI�BK�BL�BN�BO�BR�BVBXB[#B]/B^5B^5B\)B\)B_;BaHBbNBe`BhsBhsBiyBm�Bm�Bm�Bl�Bm�Bp�Br�Bv�B{�B�B�B�B�1B�DB�bB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�-B�9B�RB�^B�qB��BĜBƨB��B��B��B��B��B��B�
B�#B�`B�mB�sB�B�B�B�B��B��B��B��B��B		7B	PB	oB	�B	"�B	$�B	%�B	(�B	0!B	49B	5?B	6FB	7LB	8RB	;dB	=qB	>wB	?}B	C�B	E�B	F�B	E�B	G�B	L�B	R�B	VB	XB	ZB	ZB	ZB	[#B	[#B	\)B	\)B	]/B	^5B	_;B	_;B	bNB	dZB	ffB	iyB	l�B	n�B	o�B	q�B	s�B	s�B	t�B	v�B	w�B	y�B	z�B	|�B	� B	�B	�B	�B	�+B	�DB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�?B	�LB	�RB	�RB	�XB	�XB	�XB	�^B	�jB	�}B	��B	��B	B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�ZB	�fB	�fB	�fB	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
	7B
DB
DB
DB
JB
PB
VB
VB
\B
bB
bB
\B
bB
hB
hB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
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
%�B
%�B
%�B
&�B
&�B
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
+B
+B
+B
,B
,B
,B
,B
.B
/B
/B
0!B
0!B
1'B
0!B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
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
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
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
J�B
J�B
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
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
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
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
XB
XB
XB
XB
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
]/B
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
`BB
_;B
_;B
_;B
`BB
`BB
`BB
_;B
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
aHB
aHB
aHB
bNB
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
e`B
e`B
ffB
e`B
e`B
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
gmB
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
n�B
n�B
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B;0B9$B9$B9$B8B8B7B2�B-�B(�B&�B'�B'�B&�B)�B'�B'�B&�B&�B&�B&�B'�B'�B'�B'�B(�B+�B1�B;0B>]BDgBiDB��B�_B��B��B��B�.B�(B�"B�	B�B�@B�@B�YB�:B�4B�"B��B�.B�MB�B��B��B��B}�Bt�Br|BncB`B\�BY�BQ�BP�BK�BGzB_Bc BN�B=<B,�B*�B%�B �BMB��B�iB�vB�]B�BɆB��B�MB�FB�.B�B��B{�Bu�BncBncB_BM�BGzB8B0�B)�B.B
�iB
��B
�mB
�YB
q[B
BAB
!�B
@B
�B	��B	�QB	� B	��B	��B	��B	ѷB	�YB	�BB	��B	�B	��B	��B	�xB	� B	��B	~�B	u�B	oiB	jKB	_B	U�B	L~B	H�B	DMB	A;B	>BB	;0B	5�B	1�B	(�B	"�B	~B	xB	xB	EB	:B	B	�B		�B	�B	�B��B��B��B��B��B��B�oB�CB�B�,B��B��B��BϫB̘B�aB�B�B�B�B��B��B�QB�B��B�B}�B|�B{�B{�Bz�By�By�Bx�Bw�Bu�Br|BoOBj0BeBa�Ba�B`B\�BY�BV�BU�BT�BS�BO�BL~BHfBFYBDgBA;B?HB=<B<6B;B:*B9	B8B7B8B5B2�B5B3�B1�B1�B2�B1�B0�B+�B,�B)�B'�B$�B#�B"�B!|B vBpB~B~B~BdB]B!|B"�B%�B'�B'�B(�B*�B*�B*�B+�B+�B+�B+�B*�B+�B+�B+�B+�B+�B-�B-�B1�B3�B3�B4B6�B9$B="B@4BA;BESBGzBHfBIlBK�BL~BN�BO�BR�BU�BW�BZ�B\�B]�B]�B[�B[�B_B`�BbBe,Bh$Bh$Bi*BmCBm]BmCBl=BmCBpUBraBvzB{�B��B��B��B��B��B�B�B�&B�,B�2B�?B�QB�dB�pB�pB��B��B��B��B��B��B��B�B�B�"B�;B�MB�YB˒B�~B�~BΥBЗBԯBּB��B�B�8B�$B�0B�iB�[B�aB�nB��B��B��B��B		B	B	 B	]B	"�B	$�B	%�B	(�B	/�B	3�B	4�B	5�B	6�B	8B	;0B	=<B	>(B	?.B	CGB	EmB	FtB	EmB	G_B	L�B	R�B	U�B	W�B	Y�B	Y�B	Y�B	Z�B	Z�B	[�B	[�B	\�B	^B	^�B	^�B	a�B	dB	fB	i*B	l=B	nIB	oOB	qvB	shB	shB	t�B	vzB	w�B	y�B	z�B	|�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�@B	�2B	�9B	�?B	�YB	�EB	�eB	�WB	�dB	�jB	�pB	�vB	�vB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�	B	�	B	�B	�B	�.B	�OB	�4B	�AB	�AB	�GB	�gB	�SB	�mB	�YB	�zB	�fB	�lB	�lB	�xB	�xB	�~B	̈́B	̈́B	̈́B	ΊB	ϑB	ЗB	ЗB	ЗB	ѷB	өB	өB	өB	յB	ּB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�2B	�$B	�*B	�0B	�0B	�0B	�KB	�6B	�WB	�WB	�CB	�IB	�OB	�OB	�UB	�[B	�|B	�hB	�hB	�nB	�B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B

�B

�B
�B
B
B
B
B
B
B
B
B
B
B
 B
 B
 B
&B
&B
&B
&B
,B
,B
,B
,B
MB
2B
2B
MB
9B
2B
2B
2B
9B
9B
?B
?B
?B
?B
?B
_B
EB
EB
KB
KB
eB
QB
QB
WB
qB
xB
]B
]B
]B
xB
]B
dB
dB
�B
jB
jB
jB
jB
jB
pB
pB
pB
pB
 vB
 vB
 vB
 �B
 vB
 vB
 vB
 �B
"�B
"�B
"�B
"�B
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
%�B
%�B
%�B
&�B
&�B
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
*�B
*�B
*�B
+�B
+�B
+�B
+�B
-�B
.�B
.�B
/�B
/�B
0�B
/�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
3�B
3�B
4�B
4�B
4�B
4�B
5�B
6B
5�B
6B
5�B
7B
6�B
8B
8B
9	B
9	B
9$B
9	B
9	B
:B
:B
;0B
;B
;0B
;B
;B
;B
<B
<B
<6B
<B
<B
<B
=<B
="B
="B
>(B
>BB
>(B
>(B
?.B
@4B
@OB
@4B
@4B
@4B
@OB
@4B
@4B
@OB
@4B
@4B
A;B
BAB
B[B
CGB
CGB
CaB
CGB
CGB
CGB
CaB
CGB
CGB
CaB
CGB
DMB
DMB
DMB
DMB
DMB
ESB
ESB
ESB
ESB
FYB
FYB
FYB
FtB
G_B
G_B
HfB
H�B
H�B
HfB
H�B
HfB
HfB
IlB
IlB
I�B
IlB
I�B
IlB
I�B
I�B
IlB
J�B
J�B
JrB
JrB
KxB
KxB
L~B
L~B
L~B
L~B
L�B
L~B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
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
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
V�B
W�B
W�B
W�B
W�B
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
\�B
\�B
\�B
\�B
^B
]�B
^B
^�B
^�B
^�B
_B
^�B
_�B
_B
^�B
_B
_�B
`B
_�B
^�B
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
`B
`�B
`�B
aB
a�B
cB
cB
cB
d&B
dB
dB
d&B
dB
dB
d&B
dB
dB
dB
dB
e,B
eB
eB
eB
e,B
e,B
eB
e,B
eB
fB
eB
eB
fB
fB
fB
g8B
gB
gB
g8B
gB
gB
gB
gB
gB
gB
g8B
h$B
h$B
h$B
h$B
h$B
i*B
i*B
iDB
i*B
jKB
jKB
jKB
j0B
k6B
k6B
k6B
k6B
k6B
k6B
k6B
kQB
k6B
l=B
l=B
l=B
l=B
l=B
lWB
l=B
m]B
mCB
mCB
mCB
mCB
mCB
mCB
m]B
nIB
ncB
nI11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.57(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810070046592018100700465920181007004659201810080037352018100800373520181008003735JA  ARFMdecpA19c                                                                20181002093521  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181002003639  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181002003642  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181002003643  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181002003643  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181002003643  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181002003644  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181002003644  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181002003645  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181002003645                      G�O�G�O�G�O�                JA  ARUP                                                                        20181002005958                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181002154706  CV  JULD            G�O�G�O�F�-�                JM  ARCAJMQC2.0                                                                 20181006154659  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181006154659  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181007153735  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                