CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2020-03-10T03:39:46Z creation;2020-03-10T03:39:49Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20200310033946  20200310040236  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               EA   JA                                  2B  A   APEX                            7906                            051216                          846 @���l�1   @���l�@4�/���d�j~��#1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�33@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��Dy�D  D�fDfD� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D��D� D   D � D!  D!� D"  D"� D#  D#y�D#��D$� D%  D%� D&fD&�fD'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D,��D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DG  DG�fDHfDH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DVy�DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_y�D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D��3D�  D�@ D�|�D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�C3DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�3D�C3Dр D�� D�  D�@ DҀ D�� D�  D�@ DӃ3D�� D�  D�@ DԀ D�� D�  D�@ D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D��3D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�3D�@ Dހ D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D���D�<�D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @G�@�=p@�
=A�A#�AC�Ac�A�A�A��\A�A�A�A�A�B �HB�HB�HB�HB �HB(�HB0�HB8�HB@�HBH�HBP�HBX�HB`�HBh�HBp�HBx�HB�p�B�p�B�p�B�p�B�p�B���B���B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B���B���B�p�C 8RC8RC8RC8RC8RC
8RC8RC8RCQ�C8RC8RC8RC8RC8RC8RC8RC 8RC"8RC$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC48RC68RC88RC:8RC<8RC>�C@8RCB8RCD8RCF8RCH8RCJ8RCL8RCN8RCP8RCR8RCT8RCV8RCX8RCZ8RC\8RC^8RC`8RCb8RCd8RCf8RCh8RCj8RCl8RCn8RCp8RCr8RCt8RCv8RCx8RCz8RC|Q�C~8RC�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)D D �zDD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�D�D��DD�zDzD�DD�DD�D�D�DD�DD�DD�DD�DD�DD�DD�DD�zDzD�DD�DD�D�D�D D �D!D!�D"D"�D#D#��D$�D$�D%D%�D&zD&�zD'D'�D(D(�zD)D)�D*D*�D+D+�D,D,�D-�D-��D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�zD:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCzDC�DDDD�DEDE�DFDF�DGDG�zDHzDH�DIDI�DJDJ�DKDK�DL�DL�DMDM�DNDN�DOzDO�DPDP�DQDQ�DRDR�zDSDS�DTDT�DUDU�DVDV��DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]�D]�D^D^�D_D_��D`D`�DaDa�zDbDb�DcDc�DdDd�DeDe��DfDf�DgDg�DhDh��DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�zDyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D���D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�C�D���D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��=D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D���D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D���D��
D�
D�G
D��
D��
D�
D�G
D��
D��=D�
D�G
D��
D��
D�
D�G
D��=D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�C�D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D��D�C�D��
D��=D�
D�G
D���D���D��D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��=D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�C�D���D��
D�
D�G
D��
D��
D�
D�J=D��=D��
D�
D�G
D
D��
D�
D�G
DÇ
D��
D�
D�J=Dć
D��
D�
D�G
DŇ
D��
D�
D�G
DƇ
D��
D�
D�G
DǇ
D��
D�
D�G
Dȇ
D��
D�
D�G
Dɇ
D��
D�
D�G
Dʇ
D��
D�
D�G
Dˇ
D��
D�
D�G
Ḋ
D��
D�
D�G
D͇
D��
D�
D�G
D·
D��
D�
D�G
Dχ
D��
D�
D�G
DЇ
D��
D�
=D�J=Dч
D��
D�
D�G
D҇
D��
D�
D�G
Dӊ=D��
D�
D�G
Dԇ
D��
D�
D�G
DՃ�D��
D�
D�G
Dև
D��
D�
D�G
Dׇ
D��
D�
D�G
D؇
D��
D�
D�G
Dه
D��=D�
D�G
Dڇ
D��
D�
D�G
Dۇ
D��
D�
D�G
D܇
D��
D�
D�G
D݇
D��
D�
=D�G
Dއ
D��
D�
D�G
D߃�D��
D�
D�G
D��
D��
D�
D�G
D�
D��
D�
D�G
D�
D��
D�
D�G
D�
D��
D�
D�G
D�
D��
D�
D�G
D�
D��
D�
D�G
D�
D��
D�
D�G
D�
D��
D�
=D�G
D�
D��
D�
D�J=D�
D��
D�
D�G
D�
D��
D�
D�G
D�
D��
D��D�G
D�
D��=D�
D�G
D�
D��
D�
D�G
D�
D��
D��D�C�D�
D��
D�
D�G
D��
D��
D�
D�G
D�
D��
D��D�G
D�
D��
D�
D�G
D�
D��
D�
D�G
D�
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�G
D��
D��
D�
=D�G
D��
D��
D�
D�C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�-A�+A��A��A� �A�{A��A�%A�%A���A��A��A��A��`A��;A��
A���A���A���A���A���A���A��
A��
A���A���A���A���A���A��
A��
A��
A��A��A��#A��#A��#A��#A��A��A��A��A���A���A�ȴAȴ9A��A�ZA��TA�r�A��A���A��#A��A�`BA��A�`BA��mA���A�33A���A�"�A��-A�VA�l�A�ĜA�A���A�G�A�O�A�dZA�G�A��^A�bA�jA���A���A���A��A�bNA��A�Q�A��A��A��A�`BA��A�7LA�z�A�O�A�%A��;A��-A�p�A��TA��A�"�A�v�A��jA�p�A�O�A�/A���A���A�(�A�bA�l�A��A� �A��A���A�
=A�%A�  A�\)A���A�bNA��/A�+A}�wA|-Az��Ax��AwoAv1'Au��Au+AtAr��AqƨApM�AnjAl�AlZAi��Af��Ae�wAdr�Aa��A]��A\�AZ��AY��AW�AV=qASAR9XAQC�AP(�AN��AM�AKt�AJE�AG�AFE�AE�ADn�AB�AAXA@ĜA?��A>�+A=|�A;�A9��A8ȴA7K�A6��A6{A3�;A2n�A1��A1|�A0ZA.�yA-��A,E�A+�FA+\)A*1A'��A'l�A'"�A&JA"�/A"�A��A9XA�TA�RA��A�AdZA/A�RA=qA�^A�DA��A�A�RA��A�A$�AƨA�A7LA�HA{A�A�jAQ�A �A��A
�HA
M�A	l�AbNA{At�A��A;dAz�A{A�+A�#A��A �HA �@��@�@��@��\@���@��#@�r�@��H@�5?@�/@�\@�1@�o@�v�@�J@�O�@� �@���@�j@�S�@�o@��@�7@�9X@� �@�|�@�ff@�Q�@�;d@�v�@��#@�7L@ܼj@�t�@���@ؓu@�C�@֟�@�@���@���@҇+@҇+@���@�(�@�o@��@�%@��
@��y@ɺ^@�x�@�&�@Ȭ@�|�@ƸR@ǶF@��@�V@ˮ@���@�/@�(�@Ɵ�@���@�@�%@�;d@�~�@���@���@�@�@�hs@��u@�j@�z�@�b@���@��@��@��^@��@�p�@�x�@��7@�C�@��!@�&�@�~�@°!@þw@�Q�@�5?@��H@�+@���@��H@�M�@ř�@�O�@���@�A�@���@�
=@�V@�@�G�@�?}@���@��F@�-@�&�@�r�@�\)@�n�@���@�r�@��m@�bN@���@��@��@�A�@���@��@��m@� �@��y@��h@�hs@�X@�X@��/@���@�z�@��@�;d@�E�@���@�x�@�%@���@��`@��u@�1@�l�@�33@�
=@�+@��+@���@�&�@���@���@���@��@�Q�@�1'@� �@�(�@���@��@��@�"�@��H@��\@�@��#@��T@��@���@���@�@��#@�@���@���@� �@��;@��H@���@�M�@�{@���@��^@�{@�-@�M�@�E�@���@�p�@�&�@��@���@���@���@�bN@�ƨ@��F@��
@�`B@��@���@��@�dZ@���@�~�@��\@���@�M�@�5?@�V@���@��!@��\@�=q@��#@���@��7@�x�@�X@��@���@���@�r�@�Q�@��@�|�@�o@��@��R@�n�@�5?@��@�Ĝ@��9@�z�@��
@�33@���@��@��y@���@��+@�n�@�ff@�V@�E�@�=q@�-@�-@�J@��-@�/@�Z@� �@�1@���@�t�@�
=@��@��+@�V@���@���@�%@���@�A�@��m@���@�C�@��@��R@���@���@��+@�=q@���@�V@���@��@�z�@�9X@��;@�t�@�33@�o@��H@���@��\@�ff@�-@�J@���@��7@�V@���@��/@��9@���@��@�Z@��@��@���@���@�S�@��!@�n�@�J@��T@�@��-@�X@�/@�V@���@���@��@��`@��`@���@��@�(�@|�@�@~�@~ȴ@~ȴ@~�R@}@}p�@}`B@}O�@}?}@|��@{�F@{33@{"�@{@z��@z�\@z~�@y�@yG�@xĜ@x�u@xbN@w�@wK�@w\)@wK�@w+@v��@v5?@u�T@u�h@uO�@u/@t�/@tz�@t9X@t1@sS�@so@r��@rn�@q��@q�#@p��@p�9@p�@p �@p  @o�@o�;@ol�@o�@nȴ@n��@nv�@m�@m�-@mO�@m/@m/@lj@k�
@kƨ@kƨ@k�F@kt�@k@j�@j��@j=q@i�@i�^@ix�@h��@h�u@hb@g|�@g+@fv�@e�T@ep�@e�@d�@d��@dz�@dZ@c�
@co@b-@a�7@aG�@`�`@`�@`Q�@_�@_��@_\)@^�@^v�@]@\��@\Z@[�
@[t�@[o@Z��@Z�@Y��@YX@Y�@X�`@X�u@XQ�@W��@W�@V��@Vff@V{@U`B@T�@T�@TI�@S��@St�@So@R��@R�!@R~�@R^5@R=q@R-@R�@Q�#@Q��@QG�@PĜ@P�@PQ�@O�@O�@O��@Ol�@O�@N�y@N��@N��@N��@Nv�@Nv�@Nff@Nff@NE�@M/@L�/@Lj@K�@Ko@J��@J��@J�!@J�!@J�\@J^5@I��@Ix�@Ix�@IX@HĜ@H1'@H  @G�@G�@G�;@G�;@G�;@G��@G�P@F��@F{@E@E�-@E��@EO�@E�@D�/@D�j@D�@D�D@Dj@Dj@DZ@D9X@C�m@C�@Ct�@CdZ@C33@Co@C@B��@B�!@B��@Bn�@A�^@@��@@�9@@��@@��@@�9@@�u@@r�@@1'@?�;@?|�@?;d@?�@>�y@>ȴ@>E�@=��@=@=�-@=O�@<�@<�j@<(�@;t�@;S�@:��@:n�@:=q@:J@9G�@8��@8Q�@81'@8 �@8b@7�@7�w@7�P@7l�@7\)@7;d@6��@6v�@5�@5O�@5V@4�j@4j@4(�@3�F@3�@3o@2��@1��@1hs@1&�@0�`@0�u@0r�@0 �@/�@/�@/�;@/��@/�@.�+@-��@-@-`B@-?}@-/@-�@-V@,��@,�/@,�@,z�@,9X@+��@+ƨ@+�F@+�@+33@+@*�H@*�!@*^5@*J@)�#@)��@)hs@)G�@)&�@(Ĝ@(��@(bN@(1'@( �@(  @'�@'�P@'l�@'\)@'+@&��@&ff@&5?@&{@&{@&@%�@%�T@%�T@%�-@%�@%`B@%�@$�@$�/@$�@$Z@#�m@#�@#S�@#C�@#33@#@"�@#@"�@"�@"�H@"�H@"�H@"�!@"^5@"=q@"J@!�^@!�7@!x�@!G�@!7L@!&�@ ��@ �9@ r�@  �@�@�w@�P@\)@;d@+@�@�@��@��@E�@@�-@�@`B@/@�@j@��@�F@��@dZ@S�@33@o@��@�\@=q@-@J@��@��@x�@hs@G�@�@��@�u@bN@��@��@K�@��@�R@5?@�T@��@@��@p�@/@��@�j@j@(�@1@�
@�F@t�@33@��@��@n�@�@��@�^@��@��@��@x�@�@%@�`@��@bN@1'@  @�;@�w@�@��@|�@l�@\)@;d@��@��@v�@5?@{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�-A�+A��A��A� �A�{A��A�%A�%A���A��A��A��A��`A��;A��
A���A���A���A���A���A���A��
A��
A���A���A���A���A���A��
A��
A��
A��A��A��#A��#A��#A��#A��A��A��A��A���A���A�ȴAȴ9A��A�ZA��TA�r�A��A���A��#A��A�`BA��A�`BA��mA���A�33A���A�"�A��-A�VA�l�A�ĜA�A���A�G�A�O�A�dZA�G�A��^A�bA�jA���A���A���A��A�bNA��A�Q�A��A��A��A�`BA��A�7LA�z�A�O�A�%A��;A��-A�p�A��TA��A�"�A�v�A��jA�p�A�O�A�/A���A���A�(�A�bA�l�A��A� �A��A���A�
=A�%A�  A�\)A���A�bNA��/A�+A}�wA|-Az��Ax��AwoAv1'Au��Au+AtAr��AqƨApM�AnjAl�AlZAi��Af��Ae�wAdr�Aa��A]��A\�AZ��AY��AW�AV=qASAR9XAQC�AP(�AN��AM�AKt�AJE�AG�AFE�AE�ADn�AB�AAXA@ĜA?��A>�+A=|�A;�A9��A8ȴA7K�A6��A6{A3�;A2n�A1��A1|�A0ZA.�yA-��A,E�A+�FA+\)A*1A'��A'l�A'"�A&JA"�/A"�A��A9XA�TA�RA��A�AdZA/A�RA=qA�^A�DA��A�A�RA��A�A$�AƨA�A7LA�HA{A�A�jAQ�A �A��A
�HA
M�A	l�AbNA{At�A��A;dAz�A{A�+A�#A��A �HA �@��@�@��@��\@���@��#@�r�@��H@�5?@�/@�\@�1@�o@�v�@�J@�O�@� �@���@�j@�S�@�o@��@�7@�9X@� �@�|�@�ff@�Q�@�;d@�v�@��#@�7L@ܼj@�t�@���@ؓu@�C�@֟�@�@���@���@҇+@҇+@���@�(�@�o@��@�%@��
@��y@ɺ^@�x�@�&�@Ȭ@�|�@ƸR@ǶF@��@�V@ˮ@���@�/@�(�@Ɵ�@���@�@�%@�;d@�~�@���@���@�@�@�hs@��u@�j@�z�@�b@���@��@��@��^@��@�p�@�x�@��7@�C�@��!@�&�@�~�@°!@þw@�Q�@�5?@��H@�+@���@��H@�M�@ř�@�O�@���@�A�@���@�
=@�V@�@�G�@�?}@���@��F@�-@�&�@�r�@�\)@�n�@���@�r�@��m@�bN@���@��@��@�A�@���@��@��m@� �@��y@��h@�hs@�X@�X@��/@���@�z�@��@�;d@�E�@���@�x�@�%@���@��`@��u@�1@�l�@�33@�
=@�+@��+@���@�&�@���@���@���@��@�Q�@�1'@� �@�(�@���@��@��@�"�@��H@��\@�@��#@��T@��@���@���@�@��#@�@���@���@� �@��;@��H@���@�M�@�{@���@��^@�{@�-@�M�@�E�@���@�p�@�&�@��@���@���@���@�bN@�ƨ@��F@��
@�`B@��@���@��@�dZ@���@�~�@��\@���@�M�@�5?@�V@���@��!@��\@�=q@��#@���@��7@�x�@�X@��@���@���@�r�@�Q�@��@�|�@�o@��@��R@�n�@�5?@��@�Ĝ@��9@�z�@��
@�33@���@��@��y@���@��+@�n�@�ff@�V@�E�@�=q@�-@�-@�J@��-@�/@�Z@� �@�1@���@�t�@�
=@��@��+@�V@���@���@�%@���@�A�@��m@���@�C�@��@��R@���@���@��+@�=q@���@�V@���@��@�z�@�9X@��;@�t�@�33@�o@��H@���@��\@�ff@�-@�J@���@��7@�V@���@��/@��9@���@��@�Z@��@��@���@���@�S�@��!@�n�@�J@��T@�@��-@�X@�/@�V@���@���@��@��`@��`@���@��@�(�@|�@�@~�@~ȴ@~ȴ@~�R@}@}p�@}`B@}O�@}?}@|��@{�F@{33@{"�@{@z��@z�\@z~�@y�@yG�@xĜ@x�u@xbN@w�@wK�@w\)@wK�@w+@v��@v5?@u�T@u�h@uO�@u/@t�/@tz�@t9X@t1@sS�@so@r��@rn�@q��@q�#@p��@p�9@p�@p �@p  @o�@o�;@ol�@o�@nȴ@n��@nv�@m�@m�-@mO�@m/@m/@lj@k�
@kƨ@kƨ@k�F@kt�@k@j�@j��@j=q@i�@i�^@ix�@h��@h�u@hb@g|�@g+@fv�@e�T@ep�@e�@d�@d��@dz�@dZ@c�
@co@b-@a�7@aG�@`�`@`�@`Q�@_�@_��@_\)@^�@^v�@]@\��@\Z@[�
@[t�@[o@Z��@Z�@Y��@YX@Y�@X�`@X�u@XQ�@W��@W�@V��@Vff@V{@U`B@T�@T�@TI�@S��@St�@So@R��@R�!@R~�@R^5@R=q@R-@R�@Q�#@Q��@QG�@PĜ@P�@PQ�@O�@O�@O��@Ol�@O�@N�y@N��@N��@N��@Nv�@Nv�@Nff@Nff@NE�@M/@L�/@Lj@K�@Ko@J��@J��@J�!@J�!@J�\@J^5@I��@Ix�@Ix�@IX@HĜ@H1'@H  @G�@G�@G�;@G�;@G�;@G��@G�P@F��@F{@E@E�-@E��@EO�@E�@D�/@D�j@D�@D�D@Dj@Dj@DZ@D9X@C�m@C�@Ct�@CdZ@C33@Co@C@B��@B�!@B��@Bn�@A�^@@��@@�9@@��@@��@@�9@@�u@@r�@@1'@?�;@?|�@?;d@?�@>�y@>ȴ@>E�@=��@=@=�-@=O�@<�@<�j@<(�@;t�@;S�@:��@:n�@:=q@:J@9G�@8��@8Q�@81'@8 �@8b@7�@7�w@7�P@7l�@7\)@7;d@6��@6v�@5�@5O�@5V@4�j@4j@4(�@3�F@3�@3o@2��@1��@1hs@1&�@0�`@0�u@0r�@0 �@/�@/�@/�;@/��@/�@.�+@-��@-@-`B@-?}@-/@-�@-V@,��@,�/@,�@,z�@,9X@+��@+ƨ@+�F@+�@+33@+@*�H@*�!@*^5@*J@)�#@)��@)hs@)G�@)&�@(Ĝ@(��@(bN@(1'@( �@(  @'�@'�P@'l�@'\)@'+@&��@&ff@&5?@&{@&{@&@%�@%�T@%�T@%�-@%�@%`B@%�@$�@$�/@$�@$Z@#�m@#�@#S�@#C�@#33@#@"�@#@"�@"�@"�H@"�H@"�H@"�!@"^5@"=q@"J@!�^@!�7@!x�@!G�@!7L@!&�@ ��@ �9@ r�@  �@�@�w@�P@\)@;d@+@�@�@��@��@E�@@�-@�@`B@/@�@j@��@�F@��@dZ@S�@33@o@��@�\@=q@-@J@��@��@x�@hs@G�@�@��@�u@bN@��@��@K�@��@�R@5?@�T@��@@��@p�@/@��@�j@j@(�@1@�
@�F@t�@33@��@��@n�@�@��@�^@��@��@��@x�@�@%@�`@��@bN@1'@  @�;@�w@�@��@|�@l�@\)@;d@��@��@v�@5?@{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
E�B
E�B
E�B
E�B
E�B
D�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
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
C�B
C�B
D�B
D�B
D�B
D�B
D�B
F�B
H�B
J�B
L�B
k�B%BM�BXB[#Bm�Bu�B~�B��B�B�B$�B8RBG�BT�B]/BaHBffBhsBjBl�Bn�Bo�B|�B|�Bz�Bv�Bo�BiyBe`B_;BT�BR�BP�BN�BH�BF�BC�B@�B:^B)�B�B�B�B�B{BbBB�B�HB��B�3B��B��B��B��B�1Bt�Bn�B_;BO�B$�B�BVB
��B
�NB
��B
�^B
�3B
��B
��B
��B
�B
n�B
_;B
W
B
F�B
;dB
5?B
1'B
-B
&�B
�B
�B
DB
B	��B	��B	�mB	��B	ȴB	��B	�-B	��B	�bB	�%B	� B	u�B	jB	`BB	T�B	N�B	J�B	B�B	<jB	2-B	,B	"�B	�B	�B	hB	
=B	B	  B��B��B�B�B�ZB�;B�#B�B��B��BƨBÖB��B�}B�RB�9B�B�B��B��B��B��B��B��B��B��B�hB�B}�Bz�Bv�Bt�Bs�Br�Bq�Bq�Bp�Bp�Bo�Bo�B� B}�By�Bw�Bv�Bu�Bt�Br�Bq�Bn�Bm�Bl�Bl�Bl�BiyBk�BhsBcTBaHBcTBdZBbNBbNBffBiyBgmBffBffBdZBe`BffBffBk�BjBiyBo�Bp�Bp�Bp�Bl�Bl�Bq�Br�Bq�Br�Bq�Bt�Bs�Bt�Bx�By�B|�B|�B{�B|�B}�B}�B}�B�B�B�B�B�B�B�B�+B�1B�=B�PB�{B�{B��B��B��B��B��B�B�!B�-B�9B�9B�?B�FB�XB�jBȴB��B�NB��B��B�B�yB�ZB�#B��B��B�ZB�fB�ZB�NB�)B�)B�;B�BB�BB�NB�`B�mB�sB�B�B�B�B�B��B	B	"�B	6FB	D�B	K�B	S�B	XB	gmB	o�B	t�B	u�B	v�B	w�B	z�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�+B	�1B	�DB	�DB	�7B	�7B	�+B	�%B	�%B	�+B	�uB	��B	��B	��B	��B	��B	��B	�{B	�B	� B	}�B	~�B	~�B	� B	� B	~�B	~�B	~�B	~�B	�B	�B	�B	�+B	�7B	�=B	�=B	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�B	�-B	�9B	�XB	�dB	�jB	�}B	��B	��B	B	B	B	�qB	��B	��B	��B	B	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�
B	�NB	�`B	�HB	�HB	�NB	�NB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
1B
	7B
1B
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
PB
PB
VB
VB
bB
bB
bB
hB
oB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
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
$�B
$�B
%�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
$�B
%�B
%�B
%�B
%�B
&�B
%�B
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
(�B
)�B
+B
+B
+B
,B
,B
+B
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
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
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
6FB
6FB
7LB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
9XB
:^B
:^B
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
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
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
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
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
G�B
G�B
G�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
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
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
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
YB
YB
ZB
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
]/B
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
ffB
ffB
ffB
ffB
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
hsB
hsB
iyB
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
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
q�B
q�B
q�B
q�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
E�B
E�B
E�B
E�B
E�B
D�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
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
C�B
C�B
D�B
D�B
D�B
D�B
D�B
F�B
H�B
J�B
L�B
k�B%BM�BXB[#Bm�Bu�B~�B��B�B�B$�B8RBG�BT�B]/BaHBffBhsBjBl�Bn�Bo�B|�B|�Bz�Bv�Bo�BiyBe`B_;BT�BR�BP�BN�BH�BF�BC�B@�B:^B)�B�B�B�B�B{BbBB�B�HB��B�3B��B��B��B��B�1Bt�Bn�B_;BO�B$�B�BVB
��B
�NB
��B
�^B
�3B
��B
��B
��B
�B
n�B
_;B
W
B
F�B
;dB
5?B
1'B
-B
&�B
�B
�B
DB
B	��B	��B	�mB	��B	ȴB	��B	�-B	��B	�bB	�%B	� B	u�B	jB	`BB	T�B	N�B	J�B	B�B	<jB	2-B	,B	"�B	�B	�B	hB	
=B	B	  B��B��B�B�B�ZB�;B�#B�B��B��BƨBÖB��B�}B�RB�9B�B�B��B��B��B��B��B��B��B��B�hB�B}�Bz�Bv�Bt�Bs�Br�Bq�Bq�Bp�Bp�Bo�Bo�B� B}�By�Bw�Bv�Bu�Bt�Br�Bq�Bn�Bm�Bl�Bl�Bl�BiyBk�BhsBcTBaHBcTBdZBbNBbNBffBiyBgmBffBffBdZBe`BffBffBk�BjBiyBo�Bp�Bp�Bp�Bl�Bl�Bq�Br�Bq�Br�Bq�Bt�Bs�Bt�Bx�By�B|�B|�B{�B|�B}�B}�B}�B�B�B�B�B�B�B�B�+B�1B�=B�PB�{B�{B��B��B��B��B��B�B�!B�-B�9B�9B�?B�FB�XB�jBȴB��B�NB��B��B�B�yB�ZB�#B��B��B�ZB�fB�ZB�NB�)B�)B�;B�BB�BB�NB�`B�mB�sB�B�B�B�B�B��B	B	"�B	6FB	D�B	K�B	S�B	XB	gmB	o�B	t�B	u�B	v�B	w�B	z�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�+B	�1B	�DB	�DB	�7B	�7B	�+B	�%B	�%B	�+B	�uB	��B	��B	��B	��B	��B	��B	�{B	�B	� B	}�B	~�B	~�B	� B	� B	~�B	~�B	~�B	~�B	�B	�B	�B	�+B	�7B	�=B	�=B	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�B	�-B	�9B	�XB	�dB	�jB	�}B	��B	��B	B	B	B	�qB	��B	��B	��B	B	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�
B	�NB	�`B	�HB	�HB	�NB	�NB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
1B
	7B
1B
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
PB
PB
VB
VB
bB
bB
bB
hB
oB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
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
$�B
$�B
%�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
$�B
%�B
%�B
%�B
%�B
&�B
%�B
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
(�B
)�B
+B
+B
+B
,B
,B
+B
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
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
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
6FB
6FB
7LB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
9XB
:^B
:^B
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
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
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
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
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
G�B
G�B
G�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
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
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
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
YB
YB
ZB
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
]/B
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
ffB
ffB
ffB
ffB
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
hsB
hsB
iyB
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
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
q�B
q�B
q�B
q�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20200310123834  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200310033946  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200310033947  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200310033947  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200310033948  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200310033948  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200310033948  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200310033948  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200310033949  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200310033949                      G�O�G�O�G�O�                JA  ARUP                                                                        20200310040236                      G�O�G�O�G�O�                