CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2019-08-07T06:37:14Z creation;2019-08-07T06:37:16Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `X   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ܠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  20190807063714  20190807065626  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               /A   JA                                  2B  A   APEX                            7906                            051216                          846 @��	����1   @��
3333@0��+J�d��hr�!1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C�fC
  C  C  C  C  C  C  C  C�C�C�C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� DfD� D	  D	� D
  D
� D  D� D  D� D��D� D  D� D  D�fD  D� D  D� D  D� D  D� DfD�fD  D� D  D�fD  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DL��DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[fD[�fD\  D\� D\��D]y�D^  D^� D^��D_� D`fD`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�fD}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D��3D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D���D�@ Dƀ D�� D�  D�@ Dǀ D�� D���D�@ DȀ D�� D�  D�@ Dɀ Dɼ�D�  D�@ Dʀ D�� D�  D�@ Dˀ D��3D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�@ Dπ Dϼ�D�  D�@ DЀ D�� D�  D�<�Dр D�� D�  D�@ DҀ DҼ�D���D�@ DӀ D�� D�  D�@ Dԃ3D�� D�  D�@ D�|�D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D�  D�@ D؀ D�� D�  D�<�Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��3D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�
=A�A%�AC�Ac�A�A�A�A�A�A�A���A�B �HB�HBG�B�HB �HB(�HB0�HB8�HB@�HBH�HBP�HBX�HB`�HBh�HBp�HBx�HB�p�B�=qB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�=qB�=qB�p�B�p�B�p�Bȣ�B�p�B�p�B�p�B�p�B�=qB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�C Q�C8RC8RC8RC�C
8RC8RC8RC8RC8RC8RC8RC8RCQ�CQ�CQ�C Q�C"8RC$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC48RC68RC88RC:8RC<8RC>8RC@8RCB8RCD8RCF8RCH8RCJ8RCL8RCN8RCP8RCR8RCT8RCV8RCX8RCZ8RC\8RC^8RC`8RCb8RCdQ�Cf8RCh8RCj8RCl8RCn8RCp8RCr8RCt8RCv8RCx8RCz8RC|8RC~8RC�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�)C�)C�)C�)C�(�C�(�C�)C�)C�)C�)C�\C�)C�)C�)C�)D D �DD�DD�DD�DD�DD�DD�DzD�DzD�D	D	�D
D
�DD�DD�D�D�DD�DD�zDD�DD�DD�DD�DzD�zDD�DD�zDD�DD�DD�DzD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$��D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@��DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL��DM�DM�DNzDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�zD[zD[�zD\D\�D]�D]��D^D^�D_�D_�D`zD`�zDaDa�DbDb�DcDc�DdDd�DeDe�Df�Df�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq��DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�zD}D}�D~D~�DD�D�
D�G
D��
D��
D�
D�G
D��
D��
D�
D�J=D��
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
D��D�G
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
D��D�G
D��
D��=D�
=D�J=D��
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
D�J=D��
D��=D�
=D�G
D��
D��
D�
D�G
D��
D��
D�
=D�J=D��
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
D��D�C�D���D��
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
D
D��
D�
D�G
DÇ
D��
D�
D�G
Dć
D��
D�
D�G
DŇ
D��
D��D�G
DƇ
D��
D�
D�G
DǇ
D��
D��D�G
Dȇ
D��
D�
D�G
Dɇ
D���D�
D�G
Dʇ
D��
D�
D�G
Dˇ
D��=D�
D�G
Ḋ
D��
D�
D�G
D̓�D��
D�
D�G
D΃�D��
D�
D�G
Dχ
D���D�
D�G
DЇ
D��
D�
D�C�Dч
D��
D�
D�G
D҇
D���D��D�G
DӇ
D��
D�
D�G
DԊ=D��
D�
D�G
DՃ�D��
D�
D�G
Dև
D��
D�
D�C�D׃�D��
D�
D�G
D؇
D��
D�
D�C�Dه
D��
D�
=D�J=Dڇ
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
D��=D�
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
D�
D�G
D�
D��
D�
D�G
D��
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
D�
D�G
D��=D��p1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A��A�A���A�hA�n�A�VA�VA��TA�FA��A䕁A�hA䟾A�A䛦A䝲A��A��A��A��A�!A���A���A���A�9A��A�-A�E�A��#Aޣ�A��yA�jA��;A�Q�AҮAа!A�r�A�oA�r�A̸RA��A�$�A�Q�A�p�A�"�A�x�A�ȴA�ffA�&�A��AƲ-A�bA�x�A�9XA�=qA���A��-A���A�p�A�5?A��9A�l�A��A��wA�  A�jA�O�A��yA�XA�~�A��
A���A�x�A�O�A�$�A��A��PA���A�x�A��`A�(�A��A�~�A���A���A�Q�A���A��!A�7LA���A��A�hsA�33A���A�ƨA��A���A��A�t�A��mA��;A�v�A���A��A��A�x�A��A���A�`BA�S�A��A��HA�p�A�5?A�~�Ax�A}A{�mAz�Ay�Ar�uApJAn��AlA�Ah�DAe�mAe�FAc�;Aa�hA`r�A\n�AXQ�AW�^AW7LAU?}AR5?APQ�AO�hAN�/AN  ALZAJ=qAI\)AF�\AE;dAD=qACp�AB�9AAhsA@��A>v�A9+A6ȴA5�-A5/A3��A/�TA,^5A*ffA'��A#A"bNA!��A!`BA ĜA�\A^5A��AƨAAA
=A�Al�A��AffA�A�HA�;A�A�+AS�AQ�Ap�A
r�A�9AhsA(�A
=AZA��A\)@�@�o@���@��#@�b@�ȴ@��\@��@�  @�5?@�7L@�V@�K�@�"�@�@陚@�p�@�u@��@�V@�7L@�9@�1@���@�E�@�/@��@�z�@�bN@��@߅@ޟ�@�-@ݑh@���@�V@���@ܬ@�Q�@��
@۝�@�bN@�Q�@�(�@۾w@���@���@�X@�7L@؛�@ו�@�@�n�@Չ7@���@�C�@ӝ�@�@҇+@��#@�V@�Ĝ@�j@�b@��y@θR@θR@ΰ!@Η�@�V@͡�@�G�@�7L@���@���@���@�9X@�K�@ʧ�@��#@��`@��m@ǶF@ǥ�@ǅ@��@�=q@�^5@ř�@�O�@���@ģ�@�I�@���@å�@�K�@\@�E�@��-@�hs@���@�r�@� �@��P@�"�@��y@���@�^5@��@�/@�r�@�1@�\)@��@���@���@�hs@�G�@���@���@�  @���@�S�@�33@�|�@��@�l�@�S�@�C�@��@��y@�V@�@���@��@��
@��@���@�n�@��@�`B@�Z@�j@�A�@�ƨ@���@�1@�b@�ƨ@�dZ@�33@��@���@�ff@�J@�@���@��@�/@���@��`@���@�9X@�1@�b@��w@��+@��@�V@�r�@�l�@�@�E�@��@��T@�O�@���@�bN@�(�@�1@��@���@�ƨ@��P@�C�@��y@���@�V@�-@��@���@�hs@�V@��/@��9@��@��D@�r�@�Z@�1@��m@���@���@���@��F@���@��@�t�@�K�@�o@��y@�v�@�5?@�@�`B@�7L@�7L@�&�@��@��j@�9X@���@�;d@�33@��@��\@�5?@�7L@���@��D@�z�@�I�@�ƨ@��!@�^5@�$�@��#@���@�hs@�/@���@�r�@�9X@��m@��F@�
=@��!@���@�v�@�-@���@�@��@�G�@�%@��@��@���@��m@���@��@��H@���@�v�@�E�@�@���@��^@���@��@�p�@�`B@�O�@�7L@��@��@��@��`@��/@��j@��D@�r�@�I�@��
@���@�t�@�+@��y@�ȴ@��R@��!@���@�n�@�=q@���@��-@��7@�G�@�?}@��@��`@��j@��@���@���@�z�@�(�@��@�b@��;@�|�@�
=@���@�V@��@�{@�-@�5?@��^@���@�X@��@��@�Ĝ@���@��u@�z�@� �@��@���@�K�@���@�~�@�V@�=q@�J@���@��h@�hs@�X@�?}@�&�@�r�@�(�@�  @�P@
=@~�@~{@}@}�h@}p�@|��@|I�@{��@{o@zJ@yG�@x�`@xr�@w�@wK�@v��@v��@v�R@v��@v��@v��@v�+@vv�@v{@t��@t1@s�F@s33@r�!@qhs@p��@p��@p�@o�;@n��@n�R@n5?@mO�@mV@l�j@l1@j��@jJ@i��@ihs@iG�@hĜ@hQ�@hQ�@hA�@hA�@hQ�@hr�@hbN@g�@g�@fE�@fE�@e�@e�@d�D@a��@ahs@ahs@aX@aX@ahs@ahs@aX@ahs@ahs@ahs@ahs@ahs@aX@aX@a7L@`��@`��@`r�@` �@_�w@^�y@^ff@^{@]��@]�h@]�@\��@\��@\Z@\(�@\�@[�m@[��@[S�@["�@Z�!@Z^5@Z^5@ZM�@Z=q@Z-@ZJ@Y�@Y��@Y��@YX@Y7L@X��@Xr�@W�@W��@W|�@W;d@W
=@Vȴ@Vv�@Vv�@V$�@U�h@T�@TI�@T(�@T(�@T1@S��@S�F@SS�@SS�@S33@RM�@P��@Pb@OK�@O+@O�@N��@N�y@N�@N�R@N��@Nff@N$�@M��@M�@L�@L�/@L��@L�j@L�D@Lz�@Lj@L9X@K�m@Kƨ@K��@Kt�@KdZ@KdZ@KS�@K"�@J�@J�\@J^5@I��@Ihs@H��@HbN@HA�@Hb@G
=@F{@E��@E��@E��@E�h@D��@DZ@C�F@Ct�@B�!@A��@A�@A�#@A�^@A�7@@�`@@b@?�w@?��@?\)@?�@>��@>�+@>v�@>ff@>V@=�T@=?}@<��@<j@;�m@;dZ@;33@:��@:~�@:J@9�#@9x�@97L@9�@9�@8��@8Ĝ@8r�@8  @7�;@7�w@7�P@7�P@7|�@7l�@7;d@7+@7�@6$�@4�j@4j@4(�@333@2^5@2-@1G�@0��@0r�@0bN@01'@0  @/��@/��@/�w@/\)@/
=@.�@.��@.E�@-�T@-�-@-p�@-V@,�@,�@+��@+C�@*M�@*=q@)�@)�^@)��@)x�@)X@)&�@(��@(Ĝ@(�u@(�u@(�@(bN@(Q�@(1'@(b@'�@'�;@'��@'��@&��@%�@%O�@%�@$�D@$9X@$I�@$I�@$(�@#�
@#�F@#��@#�@#t�@#dZ@#C�@#@"�H@"�\@"^5@"=q@!��@!X@ Ĝ@ �@ 1'@ 1'@ 1'@  �@�@�P@l�@K�@+@��@$�@�T@�h@V@V@��@��@��@�D@�D@�D@j@��@C�@"�@��@��@�\@~�@�@�#@��@�#@x�@&�@��@��@Ĝ@�9@��@bN@bN@r�@r�@Q�@�;@�@l�@K�@;d@+@�@
=@�y@��@�+@�+@v�@ff@V@V@V@V@E�@$�@�T@��@��@��@��@�h@�h@O�@/@V@�j@�@��@��@�D@z�@Z@I�@�m@�m@��@�@��@�@�@�@C�@"�@33@"�@o@�@�H@��@~�@n�@n�@=q@��@��@X@7L@7L@&�@�@%@%@%@�`@�9@�u@Q�@1'@ �@b@��@|�@\)@;d@
=@�y@ȴ@��@v�@{@��@�-@�h@�@�@`B@`B@O�@�@�@�@j@(�@�@1@��@��@�
@t�@33@@@
�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A��A�A���A�hA�n�A�VA�VA��TA�FA��A䕁A�hA䟾A�A䛦A䝲A��A��A��A��A�!A���A���A���A�9A��A�-A�E�A��#Aޣ�A��yA�jA��;A�Q�AҮAа!A�r�A�oA�r�A̸RA��A�$�A�Q�A�p�A�"�A�x�A�ȴA�ffA�&�A��AƲ-A�bA�x�A�9XA�=qA���A��-A���A�p�A�5?A��9A�l�A��A��wA�  A�jA�O�A��yA�XA�~�A��
A���A�x�A�O�A�$�A��A��PA���A�x�A��`A�(�A��A�~�A���A���A�Q�A���A��!A�7LA���A��A�hsA�33A���A�ƨA��A���A��A�t�A��mA��;A�v�A���A��A��A�x�A��A���A�`BA�S�A��A��HA�p�A�5?A�~�Ax�A}A{�mAz�Ay�Ar�uApJAn��AlA�Ah�DAe�mAe�FAc�;Aa�hA`r�A\n�AXQ�AW�^AW7LAU?}AR5?APQ�AO�hAN�/AN  ALZAJ=qAI\)AF�\AE;dAD=qACp�AB�9AAhsA@��A>v�A9+A6ȴA5�-A5/A3��A/�TA,^5A*ffA'��A#A"bNA!��A!`BA ĜA�\A^5A��AƨAAA
=A�Al�A��AffA�A�HA�;A�A�+AS�AQ�Ap�A
r�A�9AhsA(�A
=AZA��A\)@�@�o@���@��#@�b@�ȴ@��\@��@�  @�5?@�7L@�V@�K�@�"�@�@陚@�p�@�u@��@�V@�7L@�9@�1@���@�E�@�/@��@�z�@�bN@��@߅@ޟ�@�-@ݑh@���@�V@���@ܬ@�Q�@��
@۝�@�bN@�Q�@�(�@۾w@���@���@�X@�7L@؛�@ו�@�@�n�@Չ7@���@�C�@ӝ�@�@҇+@��#@�V@�Ĝ@�j@�b@��y@θR@θR@ΰ!@Η�@�V@͡�@�G�@�7L@���@���@���@�9X@�K�@ʧ�@��#@��`@��m@ǶF@ǥ�@ǅ@��@�=q@�^5@ř�@�O�@���@ģ�@�I�@���@å�@�K�@\@�E�@��-@�hs@���@�r�@� �@��P@�"�@��y@���@�^5@��@�/@�r�@�1@�\)@��@���@���@�hs@�G�@���@���@�  @���@�S�@�33@�|�@��@�l�@�S�@�C�@��@��y@�V@�@���@��@��
@��@���@�n�@��@�`B@�Z@�j@�A�@�ƨ@���@�1@�b@�ƨ@�dZ@�33@��@���@�ff@�J@�@���@��@�/@���@��`@���@�9X@�1@�b@��w@��+@��@�V@�r�@�l�@�@�E�@��@��T@�O�@���@�bN@�(�@�1@��@���@�ƨ@��P@�C�@��y@���@�V@�-@��@���@�hs@�V@��/@��9@��@��D@�r�@�Z@�1@��m@���@���@���@��F@���@��@�t�@�K�@�o@��y@�v�@�5?@�@�`B@�7L@�7L@�&�@��@��j@�9X@���@�;d@�33@��@��\@�5?@�7L@���@��D@�z�@�I�@�ƨ@��!@�^5@�$�@��#@���@�hs@�/@���@�r�@�9X@��m@��F@�
=@��!@���@�v�@�-@���@�@��@�G�@�%@��@��@���@��m@���@��@��H@���@�v�@�E�@�@���@��^@���@��@�p�@�`B@�O�@�7L@��@��@��@��`@��/@��j@��D@�r�@�I�@��
@���@�t�@�+@��y@�ȴ@��R@��!@���@�n�@�=q@���@��-@��7@�G�@�?}@��@��`@��j@��@���@���@�z�@�(�@��@�b@��;@�|�@�
=@���@�V@��@�{@�-@�5?@��^@���@�X@��@��@�Ĝ@���@��u@�z�@� �@��@���@�K�@���@�~�@�V@�=q@�J@���@��h@�hs@�X@�?}@�&�@�r�@�(�@�  @�P@
=@~�@~{@}@}�h@}p�@|��@|I�@{��@{o@zJ@yG�@x�`@xr�@w�@wK�@v��@v��@v�R@v��@v��@v��@v�+@vv�@v{@t��@t1@s�F@s33@r�!@qhs@p��@p��@p�@o�;@n��@n�R@n5?@mO�@mV@l�j@l1@j��@jJ@i��@ihs@iG�@hĜ@hQ�@hQ�@hA�@hA�@hQ�@hr�@hbN@g�@g�@fE�@fE�@e�@e�@d�D@a��@ahs@ahs@aX@aX@ahs@ahs@aX@ahs@ahs@ahs@ahs@ahs@aX@aX@a7L@`��@`��@`r�@` �@_�w@^�y@^ff@^{@]��@]�h@]�@\��@\��@\Z@\(�@\�@[�m@[��@[S�@["�@Z�!@Z^5@Z^5@ZM�@Z=q@Z-@ZJ@Y�@Y��@Y��@YX@Y7L@X��@Xr�@W�@W��@W|�@W;d@W
=@Vȴ@Vv�@Vv�@V$�@U�h@T�@TI�@T(�@T(�@T1@S��@S�F@SS�@SS�@S33@RM�@P��@Pb@OK�@O+@O�@N��@N�y@N�@N�R@N��@Nff@N$�@M��@M�@L�@L�/@L��@L�j@L�D@Lz�@Lj@L9X@K�m@Kƨ@K��@Kt�@KdZ@KdZ@KS�@K"�@J�@J�\@J^5@I��@Ihs@H��@HbN@HA�@Hb@G
=@F{@E��@E��@E��@E�h@D��@DZ@C�F@Ct�@B�!@A��@A�@A�#@A�^@A�7@@�`@@b@?�w@?��@?\)@?�@>��@>�+@>v�@>ff@>V@=�T@=?}@<��@<j@;�m@;dZ@;33@:��@:~�@:J@9�#@9x�@97L@9�@9�@8��@8Ĝ@8r�@8  @7�;@7�w@7�P@7�P@7|�@7l�@7;d@7+@7�@6$�@4�j@4j@4(�@333@2^5@2-@1G�@0��@0r�@0bN@01'@0  @/��@/��@/�w@/\)@/
=@.�@.��@.E�@-�T@-�-@-p�@-V@,�@,�@+��@+C�@*M�@*=q@)�@)�^@)��@)x�@)X@)&�@(��@(Ĝ@(�u@(�u@(�@(bN@(Q�@(1'@(b@'�@'�;@'��@'��@&��@%�@%O�@%�@$�D@$9X@$I�@$I�@$(�@#�
@#�F@#��@#�@#t�@#dZ@#C�@#@"�H@"�\@"^5@"=q@!��@!X@ Ĝ@ �@ 1'@ 1'@ 1'@  �@�@�P@l�@K�@+@��@$�@�T@�h@V@V@��@��@��@�D@�D@�D@j@��@C�@"�@��@��@�\@~�@�@�#@��@�#@x�@&�@��@��@Ĝ@�9@��@bN@bN@r�@r�@Q�@�;@�@l�@K�@;d@+@�@
=@�y@��@�+@�+@v�@ff@V@V@V@V@E�@$�@�T@��@��@��@��@�h@�h@O�@/@V@�j@�@��@��@�D@z�@Z@I�@�m@�m@��@�@��@�@�@�@C�@"�@33@"�@o@�@�H@��@~�@n�@n�@=q@��@��@X@7L@7L@&�@�@%@%@%@�`@�9@�u@Q�@1'@ �@b@��@|�@\)@;d@
=@�y@ȴ@��@v�@{@��@�-@�h@�@�@`B@`B@O�@�@�@�@j@(�@�@1@��@��@�
@t�@33@@@
�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
H�B
E�B
;dB
)�B
"�B
�B
�B
�B
{B
�B
!�B
&�B
(�B
/B
:^B
8RB
<jB
A�B
C�B
E�B
G�B
N�B
[#B
o�B
�B
�{B
�B
�^B
�XB
��B
�B
�B	7B+B'�B&�B33B6FB5?BB�BC�BQ�Bl�B�7B��B�3B��B�FB�5B�sB�TB+BuB  BhB>wBL�BM�B@�BT�BM�BF�BB�BC�BE�BL�BVBYB`BBn�BgmBcTBbNBaHB`BB`BB`BB^5BZBP�B<jB7LB6FB5?B33B)�B�BVB��B��B�B�B�B�B�B�B�yB�TB�B��BɺB�XB��B�hB�Bz�Bq�BffBE�B-B
=B
�B
�B
��B
�RB
��B
iyB
O�B
B�B
7LB
&�B
+B	��B	�sB	�#B	ȴB	�LB	�?B	�'B	��B	��B	�%B	l�B	ffB	dZB	]/B	N�B	D�B	>wB	;dB	6FB	1'B	'�B	 �B	�B	bB	JB	1B	B	  B��B�B�NB�B��B��B��BȴB�}B�qB�^B��B��B��B��B��B��B��B�hB�JB�=B�+B�%B�B�B� B|�Bz�By�Bx�Bw�Bu�By�By�Bx�B�B~�By�B{�B�B�B�B�B�=B�%B�B�%B�B�B�B�B~�B�B�B�B�%B�VB�\B�\B�VB�\B�VB�\B�oB�{B��B��B��B��B�'B�RB�qB��B��BB��B��B��B��B�B�/B�NB�yB��B	+B	DB	JB	bB	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	$�B	+B	49B	8RB	:^B	=qB	?}B	@�B	B�B	C�B	H�B	I�B	I�B	I�B	I�B	J�B	N�B	Q�B	R�B	T�B	T�B	T�B	VB	YB	ZB	]/B	_;B	bNB	bNB	bNB	cTB	e`B	ffB	e`B	gmB	gmB	gmB	hsB	jB	k�B	l�B	o�B	r�B	s�B	v�B	w�B	{�B	}�B	� B	�B	�B	�B	�%B	�+B	�7B	�DB	�VB	�\B	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�9B	�LB	�XB	�XB	�XB	�dB	�qB	�}B	�}B	��B	B	ĜB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�#B	�5B	�;B	�BB	�BB	�BB	�NB	�ZB	�ZB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
	7B
	7B
	7B

=B

=B

=B

=B
DB
DB
JB
JB
PB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
hB
oB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
#�B
#�B
$�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
,B
,B
,B
,B
-B
-B
.B
-B
.B
.B
.B
.B
.B
/B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
1'B
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
49B
49B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
8RB
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
;dB
<jB
;dB
;dB
;dB
=qB
=qB
=qB
=qB
=qB
>wB
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
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
E�B
F�B
F�B
F�B
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
H�B
H�B
I�B
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
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
O�B
P�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
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
XB
YB
YB
YB
ZB
ZB
[#B
[#B
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
]/B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
_;B
`BB
`BB
`BB
aHB
bNB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
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
ffB
ffB
ffB
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
k�B
k�B
k�B
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
q�B
p�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
v�B
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
x�B
x�B
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
{�B
{�B
{�B
{�B
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
}�B
}�B
}�B
~�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
H�B
E�B
;dB
)�B
"�B
�B
�B
�B
{B
�B
!�B
&�B
(�B
/B
:^B
8RB
<jB
A�B
C�B
E�B
G�B
N�B
[#B
o�B
�B
�{B
�B
�^B
�XB
��B
�B
�B	7B+B'�B&�B33B6FB5?BB�BC�BQ�Bl�B�7B��B�3B��B�FB�5B�sB�TB+BuB  BhB>wBL�BM�B@�BT�BM�BF�BB�BC�BE�BL�BVBYB`BBn�BgmBcTBbNBaHB`BB`BB`BB^5BZBP�B<jB7LB6FB5?B33B)�B�BVB��B��B�B�B�B�B�B�B�yB�TB�B��BɺB�XB��B�hB�Bz�Bq�BffBE�B-B
=B
�B
�B
��B
�RB
��B
iyB
O�B
B�B
7LB
&�B
+B	��B	�sB	�#B	ȴB	�LB	�?B	�'B	��B	��B	�%B	l�B	ffB	dZB	]/B	N�B	D�B	>wB	;dB	6FB	1'B	'�B	 �B	�B	bB	JB	1B	B	  B��B�B�NB�B��B��B��BȴB�}B�qB�^B��B��B��B��B��B��B��B�hB�JB�=B�+B�%B�B�B� B|�Bz�By�Bx�Bw�Bu�By�By�Bx�B�B~�By�B{�B�B�B�B�B�=B�%B�B�%B�B�B�B�B~�B�B�B�B�%B�VB�\B�\B�VB�\B�VB�\B�oB�{B��B��B��B��B�'B�RB�qB��B��BB��B��B��B��B�B�/B�NB�yB��B	+B	DB	JB	bB	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	$�B	+B	49B	8RB	:^B	=qB	?}B	@�B	B�B	C�B	H�B	I�B	I�B	I�B	I�B	J�B	N�B	Q�B	R�B	T�B	T�B	T�B	VB	YB	ZB	]/B	_;B	bNB	bNB	bNB	cTB	e`B	ffB	e`B	gmB	gmB	gmB	hsB	jB	k�B	l�B	o�B	r�B	s�B	v�B	w�B	{�B	}�B	� B	�B	�B	�B	�%B	�+B	�7B	�DB	�VB	�\B	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�9B	�LB	�XB	�XB	�XB	�dB	�qB	�}B	�}B	��B	B	ĜB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�#B	�5B	�;B	�BB	�BB	�BB	�NB	�ZB	�ZB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
	7B
	7B
	7B

=B

=B

=B

=B
DB
DB
JB
JB
PB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
hB
oB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
#�B
#�B
$�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
,B
,B
,B
,B
-B
-B
.B
-B
.B
.B
.B
.B
.B
/B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
1'B
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
49B
49B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
8RB
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
;dB
<jB
;dB
;dB
;dB
=qB
=qB
=qB
=qB
=qB
>wB
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
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
E�B
F�B
F�B
F�B
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
H�B
H�B
I�B
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
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
O�B
P�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
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
XB
YB
YB
YB
ZB
ZB
[#B
[#B
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
]/B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
_;B
`BB
`BB
`BB
aHB
bNB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
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
ffB
ffB
ffB
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
k�B
k�B
k�B
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
q�B
p�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
v�B
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
x�B
x�B
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
{�B
{�B
{�B
{�B
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
}�B
}�B
}�B
~�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20190807153655  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190807063714  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190807063715  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190807063715  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190807063716  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190807063716  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190807063716  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190807063716  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190807063716  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190807063716                      G�O�G�O�G�O�                JA  ARUP                                                                        20190807065626                      G�O�G�O�G�O�                