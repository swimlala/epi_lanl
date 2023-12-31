CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:15Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Qt   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  XP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  b�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20181024140815  20181024140815  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               9A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׽���V1   @׽�q�/&@3z�G��c�C��%1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      9A   A   B   @�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B̙�B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  Dy�D  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  Dy�D��D� D  D�fDfD� D  Dy�D  D� D  Dy�D  D� D  D� D  D� D��D� D  D� D  Dy�D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&�fD'  D'y�D(  D(�fD)  D)� D*  D*� D+  D+�fD,fD,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;fD;� D<  D<� D=  D=� D>  D>� D?fD?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DPy�DP��DQ� DR  DR� DSfDS�fDTfDT� DT��DU� DV  DV�fDW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D\��D]y�D^  D^� D_  D_� D`  D`y�Da  Da� Db  Db� Dc  Dc� DdfDd�fDe  De� Df  Dy��D�EqD�e�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�
=@�
=A�A#�AC�Ac�A��\A�A�A�A�A�A�A�B �HBz�B�HB�HB �HB(�HB0�HB8�HB@�HBH�HBP�HBYG�B`�HBh�HBp�HBx�HB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B���B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�
>B�=qB�p�B�p�B�p�B�p�B�p�B��B�p�B�p�B�p�B�p�B�p�C 8RC8RC8RC8RC8RC
Q�C8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC 8RC"8RC$8RC&8RC(8RC*8RC,8RC.8RC08RC2�C48RC68RC88RC:�C<8RC>8RC@8RCB8RCD8RCF8RCHQ�CJ8RCL8RCN8RCP8RCR8RCT8RCV8RCX8RCZ8RC\8RC^8RC`8RCb8RCd8RCf8RCh8RCj8RCl8RCn8RCp�Cr8RCt8RCv8RCx8RCz8RC|8RC~8RC�)C�)C�(�C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�\C�\C�)C�)C�\C�)C�)C�)C�(�C�)C�)C�(�C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�\C�\C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�)C�)C�\C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�\C�)C�)C�(�C�)C�)C�)D D �DD�DD�DD�DD�DD�DD��DD�zDD�D	D	�D
D
�DD�DD�DD�DD�DD�zDD�DD�DD�DD��D�D�DD�zDzD�DD��DD�DD��DD�DD�DD�D�D�DD�DD��D D �D!D!�D"D"�D#D#�D$D$�D%�D%�D&D&�zD'D'��D(D(�zD)D)�D*D*�D+D+�zD,zD,�D-D-�D.D.�D/zD/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�zD6D6�D7D7�D8D8�D9D9�D:D:�D;zD;�D<D<�D=D=�D>D>�D?zD?�D@D@�zDADA�DBDB�DCDC�DDDD�DP��DQ�DQ�DRDR�DSzDS�zDTzDT�DU�DU�DVDV�zDWDW��DXDX�DYDY�DZDZ�D[D[�D\D\�D]�D]��D^D^�D_D_�D`D`��DaDa�DbDb�DcDc�DdzDd�zDeDe�DfDy��D�L{D�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aڥ�Aڧ�Aڧ�Aڧ�Aک�Aک�AڬAڬAڬAڬAڙ�AڅA�t�A�dZA�O�A�1'A�oA�bA�JA�VA�  A��HA��A��
A؟�A�?}A�l�A���AӁA�A�A��A���A�+A�ȴA�5?A�  AмjA�
=Aϣ�A�r�A�7LA��;A�ĜA��A̟�A�  A˧�A�hsAɧ�A��A���A�?}A�A�^5A��#A���A�1'A���A���A���A��DA�(�A���A��A�n�A���A���A�1A�ȴA�33A�A�A��A�oA�oA�ZA���A��mA�VA��A��;A�+A�~�A��#A��A���A��A���A��DA��;A�K�A���A�G�A���A�{A���A�dZA�&�A�JA�1'A�JA��TA���A��A�bNA��
A���A�O�A��A��A���A���A��7A�{A�Q�A��;A�1A�K�A���A��A�S�A���A�$�A{�;AxJAuK�Ar�RAp=qAm�PAk��Aj �Ah��Aep�AcS�Abr�Aa
=A^ĜA]��A[�mAY�^AX�\AV��AT�\AS33AQ��AO��ANjAL��AJ5?AI%AH�AFȴAE��AE��AD�ADjAC��AB��A@��A?/A<-A9�-A8��A7A6�HA6�A5XA4�`A4 �A3�A3;dA2ZA1`BA0~�A.�/A.Q�A,��A+C�A*�A)�A'��A&�A%�
A%`BA#��A"=qA ��A�RA�^Ax�A�HA�
A&�A�A|�A��A��A�A�hA+A�^A(�AVAr�AVAXA�7A^5A��A��A��A	dZA=qA%Ar�A|�A z�@���@��\@��@���@�X@� �@��j@�
=@���@�h@�bN@땁@��@��y@���@�w@�Ĝ@ޏ\@ݲ-@���@�ff@ם�@�t�@�"�@ӶF@҇+@��@�@��/@ˮ@��@�dZ@ЋD@أ�@؋D@�A�@�l�@�7L@ҟ�@ЋD@�  @щ7@�ff@�M�@��@Л�@���@�J@̬@�j@�9X@�ƨ@��@��@�1'@��
@ǝ�@�33@���@Ƨ�@�v�@�5?@��@���@�r�@��m@�"�@�=q@�x�@���@���@�C�@�+@�
=@���@���@�~�@�ff@�=q@�$�@�@��T@���@���@�x�@�&�@���@�b@��@��\@��@�x�@�?}@��@��@���@��@�C�@�ff@��@�7L@�&�@�&�@��@�%@��@���@�r�@���@��@�t�@��@�{@���@��@�O�@�/@���@��@��`@��/@��9@�z�@�A�@���@�S�@��@�-@��@��-@��h@��h@��@�&�@���@� �@��
@���@�|�@�K�@��@��^@�/@��@�r�@�(�@�  @�  @��@���@�\)@�@���@��+@�E�@���@��@��T@���@��h@��7@�hs@�&�@��9@��D@��@�b@�t�@�33@�o@��H@��!@��\@�~�@�^5@��@���@��D@�A�@��
@��w@��F@��F@��F@��@���@�\)@�@��\@�5?@�@���@���@�X@��`@��@�bN@�(�@���@��@���@�dZ@�;d@�v�@�@�p�@��9@��9@��@��@���@���@�j@�I�@� �@���@��P@�
=@���@���@�"h@w��@gݘ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aڥ�Aڧ�Aڧ�Aڧ�Aک�Aک�AڬAڬAڬAڬAڙ�AڅA�t�A�dZA�O�A�1'A�oA�bA�JA�VA�  A��HA��A��
A؟�A�?}A�l�A���AӁA�A�A��A���A�+A�ȴA�5?A�  AмjA�
=Aϣ�A�r�A�7LA��;A�ĜA��A̟�A�  A˧�A�hsAɧ�A��A���A�?}A�A�^5A��#A���A�1'A���A���A���A��DA�(�A���A��A�n�A���A���A�1A�ȴA�33A�A�A��A�oA�oA�ZA���A��mA�VA��A��;A�+A�~�A��#A��A���A��A���A��DA��;A�K�A���A�G�A���A�{A���A�dZA�&�A�JA�1'A�JA��TA���A��A�bNA��
A���A�O�A��A��A���A���A��7A�{A�Q�A��;A�1A�K�A���A��A�S�A���A�$�A{�;AxJAuK�Ar�RAp=qAm�PAk��Aj �Ah��Aep�AcS�Abr�Aa
=A^ĜA]��A[�mAY�^AX�\AV��AT�\AS33AQ��AO��ANjAL��AJ5?AI%AH�AFȴAE��AE��AD�ADjAC��AB��A@��A?/A<-A9�-A8��A7A6�HA6�A5XA4�`A4 �A3�A3;dA2ZA1`BA0~�A.�/A.Q�A,��A+C�A*�A)�A'��A&�A%�
A%`BA#��A"=qA ��A�RA�^Ax�A�HA�
A&�A�A|�A��A��A�A�hA+A�^A(�AVAr�AVAXA�7A^5A��A��A��A	dZA=qA%Ar�A|�A z�@���@��\@��@���@�X@� �@��j@�
=@���@�h@�bN@땁@��@��y@���@�w@�Ĝ@ޏ\@ݲ-@���@�ff@ם�@�t�@�"�@ӶF@҇+@��@�@��/@ˮ@��@�dZ@ЋD@أ�@؋D@�A�@�l�@�7L@ҟ�@ЋD@�  @щ7@�ff@�M�@��@Л�@���@�J@̬@�j@�9X@�ƨ@��@��@�1'@��
@ǝ�@�33@���@Ƨ�@�v�@�5?@��@���@�r�@��m@�"�@�=q@�x�@���@���@�C�@�+@�
=@���@���@�~�@�ff@�=q@�$�@�@��T@���@���@�x�@�&�@���@�b@��@��\@��@�x�@�?}@��@��@���@��@�C�@�ff@��@�7L@�&�@�&�@��@�%@��@���@�r�@���@��@�t�@��@�{@���@��@�O�@�/@���@��@��`@��/@��9@�z�@�A�@���@�S�@��@�-@��@��-@��h@��h@��@�&�@���@� �@��
@���@�|�@�K�@��@��^@�/@��@�r�@�(�@�  @�  @��@���@�\)@�@���@��+@�E�@���@��@��T@���@��h@��7@�hs@�&�@��9@��D@��@�b@�t�@�33@�o@��H@��!@��\@�~�@�^5@��@���@��D@�A�@��
@��w@��F@��F@��F@��@���@�\)@�@��\@�5?@�@���@���@�X@��`@��@�bN@�(�@���@��@���@�dZ@�;d@�v�@�@�p�@��9@��9@��@��@���@���@�j@�I�@� �@���@��P@�
=@���@���@�"h@w��@gݘ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
K�B
L�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
Q�B
R�B
R�B
VB
ZB
u�B
�jB
��B
��B
� B
P�B
>wB
2-B
)�B
%�B
�B
�B
33B
R�B
^5B
u�B
��B
�!B
�wB
��B
��B
�`B
�B
�B
�B
��B
=B�BE�B��B�
B�yB��BoB"�B(�B/B8RB;dB&�B�BuB�B�B�B�B�B�B�B�B-B49B5?B/B,B)�BB�BffBbNB_;BYBF�B<jB(�B�BbB+B��B  BB��B�mB�/B�B��B��B��B�qB�B��B�{B�DB�Bw�BgmBaHBZBG�B5?B �BPB%B
��B
�B
�ZB
��B
��B
{�B
n�B
aHB
E�B
!�B
+B	��B	�TB	��B	�B	��B	�jB	�LB	��B	��B	�\B	�B	r�B	k�B	dZB	[#B	T�B	K�B	A�B	7LB	0!B	&�B	�B	�B	PB	+B	B��B��B��B��B��B�B�B�ZB�#B��BĜB�}B�qB�^B�LB�?B�9B�-B�3B�-B�9B�3B�!B�B��B��B��B��B��B��B�{B�uB�hB�\B�JB�+B�B� B~�B|�By�Bv�Bs�Bq�B{�B�+Bx�B�B��B�LBƨB��B��B�B�
B��BĜB�-B��B��B�=Bz�B`BBW
BVBW
BW
BW
BVBS�BR�BQ�BQ�BR�BQ�BS�BR�BQ�BR�BW
BXBYBT�BXB[#B]/B[#BXBZB`BB_;B`BB]/B\)B[#BYB\)BcTB�VB��B��B��B��BÖB�wBBɺB��B�B�#B�#B�5B�5B�HB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	%B	%B	+B		7B	
=B	
=B	DB	JB	PB	VB	VB	\B	bB	\B	bB	oB	�B	�B	�B	#�B	&�B	(�B	+B	/B	0!B	49B	9XB	>wB	D�B	F�B	F�B	F�B	G�B	G�B	H�B	H�B	J�B	L�B	N�B	O�B	T�B	\)B	aHB	bNB	cTB	dZB	e`B	e`B	e`B	e`B	ffB	hsB	k�B	o�B	r�B	u�B	z�B	}�B	~�B	� B	� B	� B	�B	�B	�=B	�JB	�PB	�PB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�-B	�?B	�RB	�XB	�^B	�^B	�dB	�dB	�dB	�jB�B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�BB	�HB	�HB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
�B
"�B
.�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111 B
K�B
L�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
Q�B
R�B
R�B
VB
ZB
u�B
�jB
��B
��B
� B
P�B
>wB
2-B
)�B
%�B
�B
�B
33B
R�B
^5B
u�B
��B
�!B
�wB
��B
��B
�`B
�B
�B
�B
��B
=B�BE�B��B�
B�yB��BoB"�B(�B/B8RB;dB&�B�BuB�B�B�B�B�B�B�B�B-B49B5?B/B,B)�BB�BffBbNB_;BYBF�B<jB(�B�BbB+B��B  BB��B�mB�/B�B��B��B��B�qB�B��B�{B�DB�Bw�BgmBaHBZBG�B5?B �BPB%B
��B
�B
�ZB
��B
��B
{�B
n�B
aHB
E�B
!�B
+B	��B	�TB	��B	�B	��B	�jB	�LB	��B	��B	�\B	�B	r�B	k�B	dZB	[#B	T�B	K�B	A�B	7LB	0!B	&�B	�B	�B	PB	+B	B��B��B��B��B��B�B�B�ZB�#B��BĜB�}B�qB�^B�LB�?B�9B�-B�3B�-B�9B�3B�!B�B��B��B��B��B��B��B�{B�uB�hB�\B�JB�+B�B� B~�B|�By�Bv�Bs�Bq�B{�B�+Bx�B�B��B�LBƨB��B��B�B�
B��BĜB�-B��B��B�=Bz�B`BBW
BVBW
BW
BW
BVBS�BR�BQ�BQ�BR�BQ�BS�BR�BQ�BR�BW
BXBYBT�BXB[#B]/B[#BXBZB`BB_;B`BB]/B\)B[#BYB\)BcTB�VB��B��B��B��BÖB�wBBɺB��B�B�#B�#B�5B�5B�HB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	%B	%B	+B		7B	
=B	
=B	DB	JB	PB	VB	VB	\B	bB	\B	bB	oB	�B	�B	�B	#�B	&�B	(�B	+B	/B	0!B	49B	9XB	>wB	D�B	F�B	F�B	F�B	G�B	G�B	H�B	H�B	J�B	L�B	N�B	O�B	T�B	\)B	aHB	bNB	cTB	dZB	e`B	e`B	e`B	e`B	ffB	hsB	k�B	o�B	r�B	u�B	z�B	}�B	~�B	� B	� B	� B	�B	�B	�=B	�JB	�PB	�PB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�-B	�?B	�RB	�XB	�^B	�^B	�dB	�dB	�dB	�jB�B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�BB	�HB	�HB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
�B
"�B
.�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140815                              AO  ARCAADJP                                                                    20181024140815    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140815  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140815  QCF$                G�O�G�O�G�O�0               