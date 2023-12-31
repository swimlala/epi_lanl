CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:28Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  hP   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20181024140828  20181024140828  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               }A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @���1~]p1   @����Q�B@4�V��c�z�H1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      }A   A   A   @9��@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffBffB  B��B ffB(  B0  B8ffB@  BH  BO��BX  B`  BhffBo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF�CH  CJ  CL  CN�CP  CR  CT  CV  CW�fCY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&fD&� D'  D'y�D'��D(y�D(��D)y�D*  D*y�D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0�fD1fD1� D2  D2� D3fD3�fD4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DLy�DM  DM� DN  DN� DOfDO� DP  DP�fDQfDQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DVy�DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp�fDqfDq� Dr  Dry�Ds  Ds� Dt  Dt� Du  Duy�Du��Dvy�Dv��Dw� Dx  DxL�Dy�qD�D)D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@G�@�
=@�
=A�A#�AE�Ac�A�A�A�A�A�A�A�A�BG�B	G�B�HBz�B!G�B(�HB0�HB9G�B@�HBH�HBPz�BX�HB`�HBiG�Bpz�Bx�HB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�Bȣ�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�C 8RC8RC8RC8RC8RC
8RC8RC8RC8RC8RC8RC8RC8RC8RC�C8RC 8RC"8RC$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC48RC68RC88RC:8RC<8RC>8RC@8RCBQ�CDQ�CFQ�CH8RCJ8RCL8RCNQ�CP8RCR8RCT8RCV8RCX�CZ�C\8RC^8RC`8RCb8RCd8RCf8RCh8RCjQ�Cl8RCn8RCp8RCr8RCt8RCv8RCx8RCz8RC|Q�C~8RC�)C�)C�\C�)C�)C�)C�)C�)C�(�C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�)C�)C�)C�)C�)C�)D D �DD�DD�DD�DD�DD�DzD�DD�zDD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DzD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�zD&zD&�D'D'��D(�D(��D)�D)��D*D*��D+D+�D,D,�D-zD-�D.D.�D/D/�D0D0�zD1zD1�D2D2�D3zD3�zD4D4�D5zD5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�zDIDI�DJDJ�DKDK�DLDL��DMDM�DNDN�DOzDO�DPDP�zDQzDQ�DRDR�DSDS�zDTDT�DUDU�DVDV��DWDW�DXDX�DY�DY�DZDZ�D[D[�D\D\�D]D]�D^D^��D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�Dk�Dk�DlDl�DmDm�DnDn�DoDo�DpzDp�zDqzDq�DrDr��DsDs�DtDt�DuDu��Dv�Dv��Dw�Dw�DxDxZ�Dy˅D�K3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�;dA�;dA�;dA�=qA�?}A�?}A�=qA�?}A�A�A�A�A�C�A�C�A�A�A�C�A�C�A�A�A�C�A�C�A�I�A�K�A�A�r�A�E�Aպ^AԋDAЅA�"�A�|�Aȧ�A�+AĮA�A��A�bA��;A��A�ƨA��A���A��uA�7LA�A�A��A�bA��\A�S�A�1'A�r�A���A��A�|�A��A��uA�A�Q�A�jA��hA���A�M�A���A�O�A�dZA���A�{A�5?A�\)A�bA�1A���A��mA�O�A�A�A��#A��hA�5?A�5?A�+A��yA�ȴA�z�A��uA���A�G�A���A�+A���A�t�A�O�A�hsA�VA�v�A���A�7A{�;Av�jAt~�As�AsG�Ar��Aq�7Ap-AoG�An^5Am��AlE�Ah�Af(�Act�AbZAa��A`~�A_+A^I�A\ �AXVAU�AR��AQ�AO��AN��AN��AM�^AJ{AFffAC��AA��A@�9A@I�A?C�A>I�A<9XA;�A9�FA9|�A9`BA8�A7��A4��A3
=A1��A/S�A.Q�A-K�A,��A+�A*�A(-A&��A%oA#hsA"�jA"bA -A�RAA%A�
A�A�7A~�A�FA��A�7Ap�A��A�A �Ar�AE�AA�A��A+AM�A�A7LA
=A%A
��A
�A
�A	��A	oAbNA{A��Ap�A��Al�A/A�A�HA��AJA|�A�yA-A�mAl�AS�A�A ��@�o@�V@��u@��;@�S�@��@�G�@�J@�+@��-@�|�@�/@���@��
@@�%@�@�  @�~�@�7@�r�@��@��@��u@�^@�v�@�5?@��@�r�@�j@� �@ް!@��@�t�@�=q@ٲ-@��@�(�@�ȴ@�@�@Ձ@�1@Ӆ@ҧ�@�@щ7@���@�I�@���@�ƨ@ϕ�@�+@�ff@��T@͡�@�hs@�&�@̣�@�j@���@˝�@�\)@�;d@���@�hs@�A�@��m@Ǖ�@�\)@�\)@Ǖ�@Ǯ@ư!@�/@�9X@�@�V@��@���@�O�@��@�V@���@� �@�"�@��+@�v�@�E�@��@���@��@��h@��@�Z@��m@�S�@��@���@�@�?}@��9@�I�@��P@�@���@�E�@���@�&�@��j@�r�@�j@� �@��@��;@�\)@�
=@��y@��R@�$�@��-@�&�@��`@��u@�9X@�b@� �@��@�b@���@�l�@��y@�v�@�-@��T@���@�hs@�%@��@��`@�Ĝ@���@��@�Z@��@���@�=q@�ff@�-@���@�z�@��
@�t�@�ƨ@��w@�K�@��P@�33@�5?@��T@�G�@���@�9X@���@�$�@�O�@�V@�j@�9X@�A�@�r�@���@��F@��P@�l�@�S�@�C�@�;d@�"�@��!@�E�@�5?@��@���@��9@��D@��@�I�@��F@�ȴ@��\@�5?@��@��@�J@�p�@�(�@�Q�@�I�@�ƨ@���@�=q@�5?@�5?@�=q@�E�@�$�@�@��T@���@���@���@��^@���@��h@�x�@�hs@�hs@���@��/@��j@�Q�@�b@��w@���@�\)@�ȴ@��\@�=q@���@���@��7@��7@�hs@�?}@�?}@�/@�%@���@�Ĝ@���@�bN@�I�@�9X@��@���@�S�@���@���@�V@��#@�@��^@�x�@�/@�&�@���@��@��`@��j@�1'@�1@� �@��u@���@�A�@�b@��w@��F@��@���@���@��P@�l�@��H@���@��+@�~�@�=q@�@���@�7L@���@��D@�Q�@�9X@��@��;@��;@��w@�\)@�33@�
=@���@��+@�:�@zh
@gJ#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�;dA�;dA�;dA�;dA�=qA�?}A�?}A�=qA�?}A�A�A�A�A�C�A�C�A�A�A�C�A�C�A�A�A�C�A�C�A�I�A�K�A�A�r�A�E�Aպ^AԋDAЅA�"�A�|�Aȧ�A�+AĮA�A��A�bA��;A��A�ƨA��A���A��uA�7LA�A�A��A�bA��\A�S�A�1'A�r�A���A��A�|�A��A��uA�A�Q�A�jA��hA���A�M�A���A�O�A�dZA���A�{A�5?A�\)A�bA�1A���A��mA�O�A�A�A��#A��hA�5?A�5?A�+A��yA�ȴA�z�A��uA���A�G�A���A�+A���A�t�A�O�A�hsA�VA�v�A���A�7A{�;Av�jAt~�As�AsG�Ar��Aq�7Ap-AoG�An^5Am��AlE�Ah�Af(�Act�AbZAa��A`~�A_+A^I�A\ �AXVAU�AR��AQ�AO��AN��AN��AM�^AJ{AFffAC��AA��A@�9A@I�A?C�A>I�A<9XA;�A9�FA9|�A9`BA8�A7��A4��A3
=A1��A/S�A.Q�A-K�A,��A+�A*�A(-A&��A%oA#hsA"�jA"bA -A�RAA%A�
A�A�7A~�A�FA��A�7Ap�A��A�A �Ar�AE�AA�A��A+AM�A�A7LA
=A%A
��A
�A
�A	��A	oAbNA{A��Ap�A��Al�A/A�A�HA��AJA|�A�yA-A�mAl�AS�A�A ��@�o@�V@��u@��;@�S�@��@�G�@�J@�+@��-@�|�@�/@���@��
@@�%@�@�  @�~�@�7@�r�@��@��@��u@�^@�v�@�5?@��@�r�@�j@� �@ް!@��@�t�@�=q@ٲ-@��@�(�@�ȴ@�@�@Ձ@�1@Ӆ@ҧ�@�@щ7@���@�I�@���@�ƨ@ϕ�@�+@�ff@��T@͡�@�hs@�&�@̣�@�j@���@˝�@�\)@�;d@���@�hs@�A�@��m@Ǖ�@�\)@�\)@Ǖ�@Ǯ@ư!@�/@�9X@�@�V@��@���@�O�@��@�V@���@� �@�"�@��+@�v�@�E�@��@���@��@��h@��@�Z@��m@�S�@��@���@�@�?}@��9@�I�@��P@�@���@�E�@���@�&�@��j@�r�@�j@� �@��@��;@�\)@�
=@��y@��R@�$�@��-@�&�@��`@��u@�9X@�b@� �@��@�b@���@�l�@��y@�v�@�-@��T@���@�hs@�%@��@��`@�Ĝ@���@��@�Z@��@���@�=q@�ff@�-@���@�z�@��
@�t�@�ƨ@��w@�K�@��P@�33@�5?@��T@�G�@���@�9X@���@�$�@�O�@�V@�j@�9X@�A�@�r�@���@��F@��P@�l�@�S�@�C�@�;d@�"�@��!@�E�@�5?@��@���@��9@��D@��@�I�@��F@�ȴ@��\@�5?@��@��@�J@�p�@�(�@�Q�@�I�@�ƨ@���@�=q@�5?@�5?@�=q@�E�@�$�@�@��T@���@���@���@��^@���@��h@�x�@�hs@�hs@���@��/@��j@�Q�@�b@��w@���@�\)@�ȴ@��\@�=q@���@���@��7@��7@�hs@�?}@�?}@�/@�%@���@�Ĝ@���@�bN@�I�@�9X@��@���@�S�@���@���@�V@��#@�@��^@�x�@�/@�&�@���@��@��`@��j@�1'@�1@� �@��u@���@�A�@�b@��w@��F@��@���@���@��P@�l�@��H@���@��+@�~�@�=q@�@���@�7L@���@��D@�Q�@�9X@��@��;@��;@��w@�\)@�33@�
=@���@��+@�:�@zh
@gJ#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BgmBgmBgmBgmBgmBgmBgmBhsBgmBgmBgmBgmBgmBhsBhsBhsBhsBhsBhsBgmBgmBffB��B�dB�}BĜB�#B�B��B1B�B!�B#�B1'B7LB=qBH�BO�BO�BS�BQ�BP�BR�BQ�BR�BS�BT�BT�BVBT�BP�BF�B9XB49B.B7LB.B.B$�B�B%B��B�B�NB�#B��BǮB��B�3B��B�hB�%By�Be`B`BBZBJ�B-B�BB
�NB
��B
ÖB
�!B
��B
�1B
v�B
o�B
aHB
I�B
7LB
-B
�B
oB	��B	�;B	��B	��B	��B	��B	ÖB	�dB	�FB	�!B	�B	��B	�VB	�B	t�B	m�B	gmB	`BB	XB	P�B	C�B	0!B	%�B	!�B	&�B	'�B	$�B	"�B	�B	DB��B�B�sB�ZB�NB�;B�#B��B��B��B��B��BɺBŢB�}B�dB�RB�FB�?B�3B�-B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�7B�1B�%B�%B�B�B�+B�7B�1B�=B��B�9B�?B�?B�9B�9B�3B�?B�^B�qB�^B�B�?B�jBÖBȴBɺBɺB��B��B��B��B��B��B��B��B��B�B�B�5B�NB�yB�B�B�B�B��B��B��B��B��B��B	B	%B	+B	+B	1B	
=B	DB	DB	DB	PB	VB	PB	VB	VB	\B	bB	oB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	"�B	#�B	$�B	%�B	'�B	,B	/B	2-B	49B	6FB	7LB	7LB	7LB	7LB	6FB	8RB	;dB	@�B	B�B	H�B	N�B	Q�B	S�B	W
B	YB	YB	ZB	[#B	`BB	bNB	dZB	dZB	e`B	ffB	ffB	jB	k�B	l�B	m�B	p�B	s�B	v�B	x�B	y�B	|�B	~�B	� B	� B	� B	�B	�B	�B	�B	�B	� B	� B	� B	� B	�B	�B	�B	�=B	�PB	�VB	�hB	�bB	�hB	�{B	�uB	�oB	�hB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�9B	�9B	�?B	�?B	�?B	�LB	�FB	�LB	�LB	�LB	�LB	�FB	�FB	�FB	�?B	�FB	�LB	�XB	�dB	�jB	�qB	�qB	�}B	�qB	�jB	��B	��B	�}B	�}B	��B	B	B	B	B	ÖB	ÖB	ĜB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�/B	�;B	�BB	�HB	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�`B	�ZB	�`B	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
B
%B
%B
+B
+B
+B

=B
B
;B
*11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BgmBgmBgmBgmBgmBgmBgmBhsBgmBgmBgmBgmBgmBhsBhsBhsBhsBhsBhsBgmBgmBffB��B�dB�}BĜB�#B�B��B1B�B!�B#�B1'B7LB=qBH�BO�BO�BS�BQ�BP�BR�BQ�BR�BS�BT�BT�BVBT�BP�BF�B9XB49B.B7LB.B.B$�B�B%B��B�B�NB�#B��BǮB��B�3B��B�hB�%By�Be`B`BBZBJ�B-B�BB
�NB
��B
ÖB
�!B
��B
�1B
v�B
o�B
aHB
I�B
7LB
-B
�B
oB	��B	�;B	��B	��B	��B	��B	ÖB	�dB	�FB	�!B	�B	��B	�VB	�B	t�B	m�B	gmB	`BB	XB	P�B	C�B	0!B	%�B	!�B	&�B	'�B	$�B	"�B	�B	DB��B�B�sB�ZB�NB�;B�#B��B��B��B��B��BɺBŢB�}B�dB�RB�FB�?B�3B�-B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�7B�1B�%B�%B�B�B�+B�7B�1B�=B��B�9B�?B�?B�9B�9B�3B�?B�^B�qB�^B�B�?B�jBÖBȴBɺBɺB��B��B��B��B��B��B��B��B��B�B�B�5B�NB�yB�B�B�B�B��B��B��B��B��B��B	B	%B	+B	+B	1B	
=B	DB	DB	DB	PB	VB	PB	VB	VB	\B	bB	oB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	"�B	#�B	$�B	%�B	'�B	,B	/B	2-B	49B	6FB	7LB	7LB	7LB	7LB	6FB	8RB	;dB	@�B	B�B	H�B	N�B	Q�B	S�B	W
B	YB	YB	ZB	[#B	`BB	bNB	dZB	dZB	e`B	ffB	ffB	jB	k�B	l�B	m�B	p�B	s�B	v�B	x�B	y�B	|�B	~�B	� B	� B	� B	�B	�B	�B	�B	�B	� B	� B	� B	� B	�B	�B	�B	�=B	�PB	�VB	�hB	�bB	�hB	�{B	�uB	�oB	�hB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�9B	�9B	�?B	�?B	�?B	�LB	�FB	�LB	�LB	�LB	�LB	�FB	�FB	�FB	�?B	�FB	�LB	�XB	�dB	�jB	�qB	�qB	�}B	�qB	�jB	��B	��B	�}B	�}B	��B	B	B	B	B	ÖB	ÖB	ĜB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�/B	�;B	�BB	�HB	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�`B	�ZB	�`B	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
B
%B
%B
+B
+B
+B

=B
B
;B
*11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140828                              AO  ARCAADJP                                                                    20181024140828    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140828  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140828  QCF$                G�O�G�O�G�O�0               