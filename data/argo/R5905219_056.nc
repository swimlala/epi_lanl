CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2019-11-03T12:37:30Z creation;2019-11-03T12:37:34Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
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
resolution        :�o     �  Ͱ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20191103123730  20191103125449  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               8A   JA                                  2B  A   APEX                            7906                            051216                          846 @�����1   @�����@2���S��d�7KƧ�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  @���AffA@  A`  A�  A���A�  A�  A�  A�  A���A���B   B  B  BffB   B(  B0  B8  B@  BH  BP  BXffB`  Bg��Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D��Dy�D��Dy�D  D� D  D� D  Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dzy�D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�3D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D���D�@ D�� D�� D�  D�C3D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D���D�<�D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D��3D�� D�  D�<�D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ Dă3D�� D�  D�@ DŃ3D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ D�|�D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ D˃3D�� D�  D�@ D̀ D�� D�  D�@ D̓3D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ D�|�D�� D�3D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D��3D�  D�@ DԀ D�� D�  D�@ DՀ D�� D���D�@ Dր D�� D�  D�C3D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D߼�D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�#3D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @AG�@�
=@�
=A�A!�AC�Ac�A�A��\A�A�A�A�A�\A�\B �HB�HB�HBG�B �HB(�HB0�HB8�HB@�HBH�HBP�HBYG�B`�HBhz�Bp�HBx�HB�p�B�p�B�p�B�p�B���B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B���B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B��B�p�B�p�B�p�B�p�C 8RC8RC8RC8RC8RC
8RC8RC8RC8RC8RC8RC8RC8RC�C8RC8RC 8RC"Q�C$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC48RC68RC88RC:8RC<8RC>8RC@�CB8RCD8RCF8RCH8RCJ8RCL8RCN8RCP8RCR8RCT8RCV8RCX8RCZ8RC\8RC^8RC`8RCb8RCd8RCf8RCh8RCj8RCl8RCn8RCp8RCr8RCt8RCv8RCx8RCz8RC|8RC~8RC�)C�)C�)C�)C�)C�)C�\C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�\C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
zD
�DD�DD�DD�D�D��D�D��DD�DD�DD��DD�DD�zDD�DD�DD�DD�DD�DD�DD�DD�zDD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�zD(zD(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DH�DH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DP�DP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZ�DZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb��DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlzDl�DmDm�DnDn�zDoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz��D{D{�D|D|�D}D}�D~D~�DD�D�
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
D��=D��
D�
D�G
D��
D��
D��D�G
D��
D��
D�
=D�J=D��=D��
D�
D�G
D��
D��
D�
D�G
D��
D���D��D�G
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
D���D�
D�G
D���D��
D�
D�G
D��
D��
D�
D�G
D��=D��
D��D�G
D��
D��
D�
D�J=D��
D��
D��D�G
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
D��=D�
=D�G
D��
D��
D�
D�G
D��
D��
D��D�C�D��
D��
D��D�C�D��
D��
D�
=D�G
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
D��=D��
D�
D�C�D��
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
D�
=D�G
D��
D��=D�
D�G
D��
D��
D�
D�G
D
D��
D�
D�G
DÇ
D��
D�
D�G
DĊ=D��
D�
D�G
DŊ=D��
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
Dȃ�D��
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
Dˊ=D��
D�
D�G
Ḋ
D��
D�
D�G
D͊=D��
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
DЃ�D��
D�
=D�G
Dч
D��
D�
D�G
D҇
D��
D�
D�G
DӇ
D��=D�
D�G
Dԇ
D��
D�
D�G
DՇ
D��
D��D�G
Dև
D��
D�
D�J=Dׇ
D��
D�
D�G
D؇
D��
D�
D�G
Dه
D��
D�
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
D�
D�G
Dއ
D��
D�
D�G
D߇
D���D�
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
D�C�D�
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
D�C�D�
D��
D�
D�C�D��D��
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
D��
D��
D�
D�G
D��
D���D��D�G
D��
D��
D�
D�G
D��
D��
D�
D�*=D�#�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�+A�+A�+A�+A�7A�DA�DA�DA�DA�DA�DA�t�A�v�A�jA�`BA�bNA�dZA�`BA�VA�E�A�33A�(�A��A�
=A�A��A��;A�7A���A�/A�5?AٮAװ!A֡�Aԡ�A�bA���A�Aϴ9A���A�z�A� �A�=qA�XA��`AȲ-A��A�7LAƺ^A���A�
=A�|�Aİ!A��A���A�O�A�  A�|�A�oA���A��9A�;dA��PA���A�ȴA�-A���A��mA�oA���A�\)A��A��A���A�33A��A��FA��mA���A��A�ffA��+A�  A�z�A�hsA�=qA���A���A�Q�A��A��+A�n�A���A�O�A�VA���A�A��`A��A��mA���A���A�VA��A�7LA�z�A�v�A��RA�VA�A���A��TA�33A�M�A���A�bA~ZA|��Ax{Ar�!Ao%Al�\Ai�;AhVAeO�Ac�wAcS�Ab��A_A[O�AZAW�^AU��AS
=AQ7LAO\)AM�AM\)AMVALz�AK�mAJv�AHĜAG�AE��AD{ACoAB��AB1A>��A=K�A<  A:��A8��A6ZA4�A2VA/A.A-?}A,��A+��A*�jA*JA)7LA'�A&�9A%XA#��A#K�A!l�A�Al�AVAoA�A�AoA�yAI�AƨA�yA�
A%AbNA$�A�A�-A/A��A%A�Ar�A�yA��AA�A�HA�A
bNA�RAA(�AƨAl�AAn�A��A%Av�AJA�@�@���@��j@�A�@�G�@�G�@���@���@��-@��F@���@�ff@�9X@��u@��@�V@��^@�hs@�V@���@�P@�-@��D@�S�@�bN@�P@�V@��@�@�hs@�X@�O�@�Ĝ@�@�j@�o@���@��`@��
@�ȴ@��@߾w@�x�@�`B@�V@�5?@ٙ�@ؼj@�l�@��@�@�=q@�7L@Դ9@��m@�K�@�ȴ@�v�@�v�@�v�@�=q@���@ЋD@��;@�C�@���@�n�@�5?@���@�O�@�Ĝ@˾w@˅@�t�@���@ə�@ȴ9@�9X@Ǿw@�+@�l�@�@�$�@�v�@�n�@�E�@��@Ų-@�X@�/@��@ģ�@�bN@�  @�dZ@�
=@°!@�@�?}@��@�Ĝ@�Z@��;@���@��@�@�{@���@��^@�hs@��/@�Q�@��@�K�@��H@��@���@��@�G�@�(�@���@�;d@�o@��R@�=q@��@��@�&�@���@� �@���@�o@�;d@�
=@���@��@�J@�M�@�@��T@��#@��#@��#@���@��7@�x�@�&�@��9@�Q�@�b@��w@�dZ@���@���@���@�5?@��@��/@��@�/@�z�@�l�@�K�@�t�@�"�@��@���@�~�@�E�@�@���@���@��7@���@�bN@�Q�@�1@�|�@�\)@�33@�"�@�o@��@�ȴ@��+@��@���@�`B@�%@��j@���@�bN@�b@�  @��@��m@���@���@�C�@�+@�33@�33@�33@�"�@��@��@��H@���@���@��+@�~�@�ff@�E�@�$�@���@��-@��@��D@�(�@�1@��m@�|�@�;d@�o@��H@�ff@�V@�@���@�`B@��@��@��@��/@��9@���@�9X@�ƨ@���@��@�l�@�K�@��y@�v�@�E�@�=q@��@��#@��h@�O�@�&�@��@�V@���@��u@�Q�@�1'@��@�  @��
@�\)@�"�@��y@���@��\@�-@�@���@�p�@�/@��j@�z�@�A�@��@��@�t�@�\)@���@�~�@�=q@��@�p�@�X@�/@�Ĝ@�bN@�9X@� �@���@��
@���@�+@��H@���@�@��@�?}@���@���@��D@�r�@�I�@��@���@���@�33@��y@���@�n�@�-@�@��@��-@�7L@���@�r�@� �@���@��m@��;@��@�|�@�C�@��H@���@�V@�{@��#@�@��7@�G�@��9@�r�@�1'@��@��@��
@��@�dZ@�K�@�ȴ@�V@���@���@�G�@�%@�Ĝ@�r�@�bN@�I�@� �@�b@�1@�;@��@K�@~��@~�R@~��@~V@}�@}`B@|��@{��@{33@{o@{@z~�@y��@yG�@x�`@x��@xb@w\)@v��@vȴ@v�R@vV@t��@tZ@tI�@t9X@t(�@t1@s��@s�@rn�@r�@q��@q�^@q�7@q%@p��@p�`@pĜ@pQ�@o��@o\)@n��@n5?@m@mV@m�@l��@lz�@k�m@k��@k"�@j�@j~�@iX@h�9@h �@g�@f�y@f�R@fff@e��@e�@d9X@b�@b�\@b�\@b^5@a��@`��@_�;@^V@]`B@]�@\�/@\I�@\1@[�F@[�@[t�@[o@Zn�@Z-@Y�^@X��@X �@Wl�@V��@V��@Vff@V{@U@U�@T�@T1@S�
@SC�@R�!@R-@Q�@Q�^@Q7L@Q�@P�`@P��@P�u@O�@O��@OK�@N�y@Nff@N$�@M�-@L��@Lz�@L�@K��@KC�@J�H@J��@J�\@I�^@I7L@H��@HA�@Hb@Hb@G�P@G+@G
=@F�R@F�+@FE�@F{@E@E�-@E�@D�j@D(�@Ct�@CS�@CS�@CC�@B��@BM�@B-@BJ@A�#@A��@Ax�@AG�@@��@@�9@@�u@@bN@?�@?�P@?;d@>��@>V@>@=��@=p�@=/@=V@<�@<�/@<��@<�j@<�j@<�j@<�j@<�j@<�@<j@;�
@;��@;t�@;o@:��@9��@9G�@8��@8��@8bN@8 �@7�w@7K�@7�@6�y@6��@6V@5�T@5`B@5�@4�D@49X@49X@4(�@3��@3�
@3��@3�@3"�@2�@2�@2�@2��@2~�@2�@1�^@1�7@17L@0�`@0Ĝ@0r�@01'@/�;@/�@/��@/+@.�@.�R@.��@.�+@.ff@-�T@-��@-`B@,�@,�D@,(�@+ƨ@+�@+dZ@*�H@*~�@*^5@*M�@*=q@*�@)��@)G�@)%@(Ĝ@(�@(1'@'�P@'l�@'�@&�R@&ff@%�@%�@%`B@%/@$�/@$��@$j@#��@#ƨ@#��@#dZ@"�H@"�\@"-@!�#@!�7@ ��@ bN@�w@|�@�@@�-@��@�h@p�@?}@��@��@�D@(�@ƨ@�F@��@�@dZ@�@~�@�@��@�^@��@�7@hs@�@�`@Ĝ@bN@ �@�@\)@K�@+@
=@ȴ@�R@v�@V@$�@�@��@�-@?}@�/@�j@�@��@j@I�@1@�@�@t�@t�@t�@S�@33@33@@��@^5@^5@-@J@��@�@��@��@��@��@��@��@��@�7@X@G�@7L@�@%@��@��@��@�u@�@bN@ �@  @�@�@�P@\)@�@��@ȴ@�R@�R@��@��@v�@ff@V@E�@@��@��@�@p�@p�@`B@`B@O�@?}@?}@�@�@z�@z�@I�@(�@(�@(�@�@1@1@��@ƨ@�@t�@@
��@
n�@
�@	�@	�^@	G�@	�@	%@��@��@�@r�@A�@b@�;@�;@�w@�@K�@��@�y@�y@�@�R@��@�+@ff@5?@{@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�+A�+A�+A�+A�7A�DA�DA�DA�DA�DA�DA�t�A�v�A�jA�`BA�bNA�dZA�`BA�VA�E�A�33A�(�A��A�
=A�A��A��;A�7A���A�/A�5?AٮAװ!A֡�Aԡ�A�bA���A�Aϴ9A���A�z�A� �A�=qA�XA��`AȲ-A��A�7LAƺ^A���A�
=A�|�Aİ!A��A���A�O�A�  A�|�A�oA���A��9A�;dA��PA���A�ȴA�-A���A��mA�oA���A�\)A��A��A���A�33A��A��FA��mA���A��A�ffA��+A�  A�z�A�hsA�=qA���A���A�Q�A��A��+A�n�A���A�O�A�VA���A�A��`A��A��mA���A���A�VA��A�7LA�z�A�v�A��RA�VA�A���A��TA�33A�M�A���A�bA~ZA|��Ax{Ar�!Ao%Al�\Ai�;AhVAeO�Ac�wAcS�Ab��A_A[O�AZAW�^AU��AS
=AQ7LAO\)AM�AM\)AMVALz�AK�mAJv�AHĜAG�AE��AD{ACoAB��AB1A>��A=K�A<  A:��A8��A6ZA4�A2VA/A.A-?}A,��A+��A*�jA*JA)7LA'�A&�9A%XA#��A#K�A!l�A�Al�AVAoA�A�AoA�yAI�AƨA�yA�
A%AbNA$�A�A�-A/A��A%A�Ar�A�yA��AA�A�HA�A
bNA�RAA(�AƨAl�AAn�A��A%Av�AJA�@�@���@��j@�A�@�G�@�G�@���@���@��-@��F@���@�ff@�9X@��u@��@�V@��^@�hs@�V@���@�P@�-@��D@�S�@�bN@�P@�V@��@�@�hs@�X@�O�@�Ĝ@�@�j@�o@���@��`@��
@�ȴ@��@߾w@�x�@�`B@�V@�5?@ٙ�@ؼj@�l�@��@�@�=q@�7L@Դ9@��m@�K�@�ȴ@�v�@�v�@�v�@�=q@���@ЋD@��;@�C�@���@�n�@�5?@���@�O�@�Ĝ@˾w@˅@�t�@���@ə�@ȴ9@�9X@Ǿw@�+@�l�@�@�$�@�v�@�n�@�E�@��@Ų-@�X@�/@��@ģ�@�bN@�  @�dZ@�
=@°!@�@�?}@��@�Ĝ@�Z@��;@���@��@�@�{@���@��^@�hs@��/@�Q�@��@�K�@��H@��@���@��@�G�@�(�@���@�;d@�o@��R@�=q@��@��@�&�@���@� �@���@�o@�;d@�
=@���@��@�J@�M�@�@��T@��#@��#@��#@���@��7@�x�@�&�@��9@�Q�@�b@��w@�dZ@���@���@���@�5?@��@��/@��@�/@�z�@�l�@�K�@�t�@�"�@��@���@�~�@�E�@�@���@���@��7@���@�bN@�Q�@�1@�|�@�\)@�33@�"�@�o@��@�ȴ@��+@��@���@�`B@�%@��j@���@�bN@�b@�  @��@��m@���@���@�C�@�+@�33@�33@�33@�"�@��@��@��H@���@���@��+@�~�@�ff@�E�@�$�@���@��-@��@��D@�(�@�1@��m@�|�@�;d@�o@��H@�ff@�V@�@���@�`B@��@��@��@��/@��9@���@�9X@�ƨ@���@��@�l�@�K�@��y@�v�@�E�@�=q@��@��#@��h@�O�@�&�@��@�V@���@��u@�Q�@�1'@��@�  @��
@�\)@�"�@��y@���@��\@�-@�@���@�p�@�/@��j@�z�@�A�@��@��@�t�@�\)@���@�~�@�=q@��@�p�@�X@�/@�Ĝ@�bN@�9X@� �@���@��
@���@�+@��H@���@�@��@�?}@���@���@��D@�r�@�I�@��@���@���@�33@��y@���@�n�@�-@�@��@��-@�7L@���@�r�@� �@���@��m@��;@��@�|�@�C�@��H@���@�V@�{@��#@�@��7@�G�@��9@�r�@�1'@��@��@��
@��@�dZ@�K�@�ȴ@�V@���@���@�G�@�%@�Ĝ@�r�@�bN@�I�@� �@�b@�1@�;@��@K�@~��@~�R@~��@~V@}�@}`B@|��@{��@{33@{o@{@z~�@y��@yG�@x�`@x��@xb@w\)@v��@vȴ@v�R@vV@t��@tZ@tI�@t9X@t(�@t1@s��@s�@rn�@r�@q��@q�^@q�7@q%@p��@p�`@pĜ@pQ�@o��@o\)@n��@n5?@m@mV@m�@l��@lz�@k�m@k��@k"�@j�@j~�@iX@h�9@h �@g�@f�y@f�R@fff@e��@e�@d9X@b�@b�\@b�\@b^5@a��@`��@_�;@^V@]`B@]�@\�/@\I�@\1@[�F@[�@[t�@[o@Zn�@Z-@Y�^@X��@X �@Wl�@V��@V��@Vff@V{@U@U�@T�@T1@S�
@SC�@R�!@R-@Q�@Q�^@Q7L@Q�@P�`@P��@P�u@O�@O��@OK�@N�y@Nff@N$�@M�-@L��@Lz�@L�@K��@KC�@J�H@J��@J�\@I�^@I7L@H��@HA�@Hb@Hb@G�P@G+@G
=@F�R@F�+@FE�@F{@E@E�-@E�@D�j@D(�@Ct�@CS�@CS�@CC�@B��@BM�@B-@BJ@A�#@A��@Ax�@AG�@@��@@�9@@�u@@bN@?�@?�P@?;d@>��@>V@>@=��@=p�@=/@=V@<�@<�/@<��@<�j@<�j@<�j@<�j@<�j@<�@<j@;�
@;��@;t�@;o@:��@9��@9G�@8��@8��@8bN@8 �@7�w@7K�@7�@6�y@6��@6V@5�T@5`B@5�@4�D@49X@49X@4(�@3��@3�
@3��@3�@3"�@2�@2�@2�@2��@2~�@2�@1�^@1�7@17L@0�`@0Ĝ@0r�@01'@/�;@/�@/��@/+@.�@.�R@.��@.�+@.ff@-�T@-��@-`B@,�@,�D@,(�@+ƨ@+�@+dZ@*�H@*~�@*^5@*M�@*=q@*�@)��@)G�@)%@(Ĝ@(�@(1'@'�P@'l�@'�@&�R@&ff@%�@%�@%`B@%/@$�/@$��@$j@#��@#ƨ@#��@#dZ@"�H@"�\@"-@!�#@!�7@ ��@ bN@�w@|�@�@@�-@��@�h@p�@?}@��@��@�D@(�@ƨ@�F@��@�@dZ@�@~�@�@��@�^@��@�7@hs@�@�`@Ĝ@bN@ �@�@\)@K�@+@
=@ȴ@�R@v�@V@$�@�@��@�-@?}@�/@�j@�@��@j@I�@1@�@�@t�@t�@t�@S�@33@33@@��@^5@^5@-@J@��@�@��@��@��@��@��@��@��@�7@X@G�@7L@�@%@��@��@��@�u@�@bN@ �@  @�@�@�P@\)@�@��@ȴ@�R@�R@��@��@v�@ff@V@E�@@��@��@�@p�@p�@`B@`B@O�@?}@?}@�@�@z�@z�@I�@(�@(�@(�@�@1@1@��@ƨ@�@t�@@
��@
n�@
�@	�@	�^@	G�@	�@	%@��@��@�@r�@A�@b@�;@�;@�w@�@K�@��@�y@�y@�@�R@��@�+@ff@5?@{@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�XBB9XB_;BhsBjBm�By�By�Bv�B�{B��B~�B�+B�oB��B�jB��BǮB��B�;B�B�BB�)B�HBuB'�BE�BD�BG�B=qBE�BM�BN�BR�BVBW
B\)B\)B\)Be`BgmBaHB\)BS�BQ�BO�BK�BH�BC�B:^B8RB33B2-B�B�BbBbBB��B��B�mB�#B��BȴB��B�-B��B�+B{�Bp�Be`BVBG�B.B&�B�B�B1B
�B
�HB
�)B
�B
��B
ĜB
��B
�uB
�B
v�B
l�B
\)B
>wB
�B
B	��B	�ZB	�B	ĜB	�RB	�-B	�B	��B	�1B	~�B	r�B	k�B	\)B	T�B	K�B	D�B	@�B	?}B	<jB	8RB	33B	-B	%�B	�B	�B	�B	�B	�B	hB	+B	  B��B��B�B�sB�TB�#B��B��B��B��BɺBŢBÖB��B�dB�XB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�VB�DB�=B�1B�=B�VB�hB��B��B��B��B��B��B�uB�\B�JB�PB�bB��B��B��B��B��B��B��B��BŢB��B�B�B�B�B�#B�/B�HB�NB�HB�NB�5B�;B�HB�TB�`B�`B�fB�fB�B��B��B	B	+B	1B	+B	
=B	\B	{B	�B	�B	�B	(�B	'�B	)�B	33B	49B	49B	:^B	?}B	A�B	D�B	F�B	I�B	J�B	K�B	K�B	L�B	S�B	W
B	\)B	`BB	dZB	ffB	gmB	gmB	gmB	jB	n�B	n�B	n�B	q�B	w�B	|�B	� B	�B	�1B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�9B	�FB	�XB	�^B	�jB	�jB	�jB	�jB	�}B	��B	��B	B	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	��B	��B	�
B	�
B	�
B	�
B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�;B	�;B	�;B	�HB	�TB	�ZB	�ZB	�`B	�mB	�sB	�mB	�mB	�sB	�`B	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
	7B
	7B
	7B

=B
	7B

=B

=B
DB
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
VB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
+B
,B
,B
,B
,B
,B
,B
,B
-B
,B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
33B
49B
5?B
5?B
5?B
5?B
6FB
7LB
6FB
6FB
7LB
9XB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
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
:^B
;dB
;dB
=qB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
?}B
?}B
?}B
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
C�B
C�B
C�B
C�B
C�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
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
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
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
S�B
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
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
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
`BB
`BB
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
bNB
bNB
bNB
bNB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
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
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
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
t�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
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
x�B
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
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�XBB9XB_;BhsBjBm�By�By�Bv�B�{B��B~�B�+B�oB��B�jB��BǮB��B�;B�B�BB�)B�HBuB'�BE�BD�BG�B=qBE�BM�BN�BR�BVBW
B\)B\)B\)Be`BgmBaHB\)BS�BQ�BO�BK�BH�BC�B:^B8RB33B2-B�B�BbBbBB��B��B�mB�#B��BȴB��B�-B��B�+B{�Bp�Be`BVBG�B.B&�B�B�B1B
�B
�HB
�)B
�B
��B
ĜB
��B
�uB
�B
v�B
l�B
\)B
>wB
�B
B	��B	�ZB	�B	ĜB	�RB	�-B	�B	��B	�1B	~�B	r�B	k�B	\)B	T�B	K�B	D�B	@�B	?}B	<jB	8RB	33B	-B	%�B	�B	�B	�B	�B	�B	hB	+B	  B��B��B�B�sB�TB�#B��B��B��B��BɺBŢBÖB��B�dB�XB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�VB�DB�=B�1B�=B�VB�hB��B��B��B��B��B��B�uB�\B�JB�PB�bB��B��B��B��B��B��B��B��BŢB��B�B�B�B�B�#B�/B�HB�NB�HB�NB�5B�;B�HB�TB�`B�`B�fB�fB�B��B��B	B	+B	1B	+B	
=B	\B	{B	�B	�B	�B	(�B	'�B	)�B	33B	49B	49B	:^B	?}B	A�B	D�B	F�B	I�B	J�B	K�B	K�B	L�B	S�B	W
B	\)B	`BB	dZB	ffB	gmB	gmB	gmB	jB	n�B	n�B	n�B	q�B	w�B	|�B	� B	�B	�1B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�9B	�FB	�XB	�^B	�jB	�jB	�jB	�jB	�}B	��B	��B	B	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	��B	��B	�
B	�
B	�
B	�
B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�;B	�;B	�;B	�HB	�TB	�ZB	�ZB	�`B	�mB	�sB	�mB	�mB	�sB	�`B	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
	7B
	7B
	7B

=B
	7B

=B

=B
DB
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
VB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
+B
,B
,B
,B
,B
,B
,B
,B
-B
,B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
33B
49B
5?B
5?B
5?B
5?B
6FB
7LB
6FB
6FB
7LB
9XB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
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
:^B
;dB
;dB
=qB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
?}B
?}B
?}B
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
C�B
C�B
C�B
C�B
C�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
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
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
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
S�B
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
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
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
`BB
`BB
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
bNB
bNB
bNB
bNB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
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
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
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
t�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
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
x�B
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
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20191103213724  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191103123730  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191103123731  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191103123732  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191103123733  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191103123733  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191103123733  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191103123733  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191103123734  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191103123734                      G�O�G�O�G�O�                JA  ARUP                                                                        20191103125449                      G�O�G�O�G�O�                