CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:31Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140831  20181024140831  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��%#W�1   @��%�8�P@5"I�^5�c�z�G�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @&ff@�  @�  A��A   A@  A^ffA�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bo��Bx  B�33B�33B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fDfD� DfD� D��D� D	  D	� D
fD
� D  D� D��Dy�D��D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D��D� D  D� D  D�fDfD�fD  D� DfD�fDfD� D  D� D  D� D  Dy�D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$�fD%  D%� D&fD&�fD'  D'� D(  D(� D)  D)� D*fD*�fD+  D+� D,  D,� D,��D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3y�D3��D4� D5  D5� D6  D6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG� DH  DH� DH��DI� DI��DJ� DK  DKy�DL  DLy�DL��DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DU��DV� DW  DW� DX  DX� DY  DY� DZfDZ� D[  D[�fD\fD\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� DcfDc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl�fDm  Dm�fDn  Dn� Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Duy�Dv  Dv� Dw  Dw� Dw�3Dy|�D�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @4z�@�
=@�
=A�A#�AC�Aa�A�A�A���A�A�A�A�A�B �HB�HB�HB�HB �HB(�HB0�HB8�HB@�HBH�HBP�HBX�HBaG�Bh�HBpz�Bx�HB���B���B�=qB�p�B�p�B���B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�=qB�p�B�p�B�p�B�=qB�=qB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B���B�p�B�p�C 8RC8RC8RC8RC8RC
8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC 8RC"8RC$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC48RC68RC88RC:8RC<8RC>8RC@8RCB8RCD8RCF8RCH8RCJ8RCL8RCN�CP8RCR8RCT8RCV8RCX8RCZ�C\8RC^8RC`8RCb8RCd8RCf8RCh8RCj8RCl8RCn8RCp8RCr8RCt�Cv�Cx8RCz8RC|8RC~8RC�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�\C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�)C�)C�(�C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�\C�)C�(�C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�\C�\C�\C�)C�(�C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)D D �DD�DD�DD�DD�DD�zDzD�DzD�D�D�D	D	�D
zD
�DD�D�D��D�D�DD�DD�zDzD�DD�DD�DD�DD�D�D�DD�DD�zDzD�zDD�DzD�zDzD�DD�DD�DD��DD�D D �D!D!��D"D"�D#D#�D$D$�zD%D%�D&zD&�zD'D'�D(D(�D)D)�D*zD*�zD+D+�D,D,�D-�D-�D.�D.�D/D/�D0D0�D1D1�D2D2�D3D3��D4�D4�D5D5�D6D6�zD7D7�D8D8�D9D9�D:D:�D;D;�D<D<�zD=D=��D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DF�DF�DGDG�DHDH�DI�DI�DJ�DJ�DKDK��DLDL��DM�DM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUzDU�DV�DV�DWDW�DXDX�DYDY�DZzDZ�D[D[�zD\zD\�zD]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DczDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkzDk�DlDl�zDmDm�zDnDn�Do�Do�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu��DvDv�DwDw�Dw�GDy��D�7
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oA�bA�VA�%A�%A�1A�1A�VA�1A�
=A�bA�{A��A� �A��A��A� �A�"�A�"�A�"�A�"�A�$�A�$�A�&�A�+A�+A�$�A�VA�G�A�
=A���A��mAǙ�A´9A�33A�5?A���A��A�JA���A�v�A�/A�{A�"�A�$�A�n�A�?}A�ĜA��A�^5A��A�^5A��A�K�A��A��A�;dA��#A�+A��RA��A��`A��A��!A��A���A���A��jA�$�A���A�/A�\)A�I�A�VA��A�n�A�+A��`A��wA}�PA|-A{/Ay�Ax��AxbAv��Au`BAtv�Ar�Ao�#AnE�AljAk
=Ai��AgAc�-Ab�Ab-Aa��Aa%A`5?A_
=A]�A\��AZ5?AX�AWƨAV�AU��APQ�AM\)ALffAK�AK;dAJ�+AI�AG+ADȴAC�
AC�AB(�AAA?O�A>��A=�mA=O�A<�jA;�;A:r�A9�#A8�A6�!A5dZA3�A2��A2jA1�A133A/�
A.�yA-�A+�A*�/A*�\A)\)A(�+A(n�A(ffA(I�A(  A'"�A%��A#�mA"-A!"�AdZAffA��A�jA��A�A�mA��A�7A�Az�A�#A��A��A��A�DA`BA=qAl�A^5A�mAC�A�HA�9AQ�A��AA�\A�A��A��AO�A�9AffA	�
A��A�FA�hAXA�RAn�A^5Az�AI�A�;A��A��A?}AQ�A �A �@�33@���@�{@��u@�x�@���@��@��+@�A�@��@��@�J@���@�@�-@�O�@��@柾@䛦@��H@�^5@�@�/@�l�@ޟ�@�-@���@�7L@�1'@ڟ�@��@���@���@�dZ@��#@ԋD@�9X@�S�@ҸR@�~�@�E�@�-@�@�?}@�j@�+@Η�@�{@͑h@���@�1'@���@˾w@�l�@��@��@��@�V@Ĭ@�|�@�@�=q@�G�@���@��;@�-@��D@���@��@���@�n�@�~�@�v�@���@��h@�+@��T@�E�@���@�j@���@�r�@� �@�ƨ@��@�b@��F@��@��H@�$�@�$�@�^5@���@�ȴ@��H@���@��+@��\@���@���@��+@���@�5?@�J@�J@�-@��@��@��@�ƨ@�ƨ@�|�@�@�ȴ@��\@�J@��^@���@��h@�p�@�?}@���@��/@��/@��D@�Q�@�b@��@��
@�ƨ@���@�33@�@�ȴ@��R@�-@���@���@�z�@�Z@��@�\)@�+@���@��@�ȴ@��!@�n�@�M�@�$�@��@��h@�X@���@���@��@���@��@��9@��u@���@�1@�dZ@���@�  @�  @��
@���@�dZ@�\)@�\)@�\)@�;d@�;d@�;d@�K�@���@���@�
=@��@�
=@�$�@�7L@���@���@�hs@��@� �@���@��m@�\)@�+@���@��H@��+@�{@�@�@��@���@��j@�Q�@�  @���@�l�@�ƨ@� �@�Z@��@��@�V@�%@��@�z�@��@�  @��
@�C�@���@���@�I�@�Z@�1'@��@�
=@�-@�p�@�?}@��@��/@�z�@�A�@��@��F@�t�@���@�v�@�5?@�@��T@�@���@��@�p�@�hs@�O�@�7L@�%@���@�%@��@���@���@���@��@�j@�Z@�Z@�Z@�A�@��;@�l�@�+@��@��H@���@��!@��\@�~�@�n�@�ff@��R@��\@��+@�^5@��@�5?@�{@�=q@�@��-@���@���@�G�@�&�@�%@��@��@��`@��u@��A@p�?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�oA�bA�VA�%A�%A�1A�1A�VA�1A�
=A�bA�{A��A� �A��A��A� �A�"�A�"�A�"�A�"�A�$�A�$�A�&�A�+A�+A�$�A�VA�G�A�
=A���A��mAǙ�A´9A�33A�5?A���A��A�JA���A�v�A�/A�{A�"�A�$�A�n�A�?}A�ĜA��A�^5A��A�^5A��A�K�A��A��A�;dA��#A�+A��RA��A��`A��A��!A��A���A���A��jA�$�A���A�/A�\)A�I�A�VA��A�n�A�+A��`A��wA}�PA|-A{/Ay�Ax��AxbAv��Au`BAtv�Ar�Ao�#AnE�AljAk
=Ai��AgAc�-Ab�Ab-Aa��Aa%A`5?A_
=A]�A\��AZ5?AX�AWƨAV�AU��APQ�AM\)ALffAK�AK;dAJ�+AI�AG+ADȴAC�
AC�AB(�AAA?O�A>��A=�mA=O�A<�jA;�;A:r�A9�#A8�A6�!A5dZA3�A2��A2jA1�A133A/�
A.�yA-�A+�A*�/A*�\A)\)A(�+A(n�A(ffA(I�A(  A'"�A%��A#�mA"-A!"�AdZAffA��A�jA��A�A�mA��A�7A�Az�A�#A��A��A��A�DA`BA=qAl�A^5A�mAC�A�HA�9AQ�A��AA�\A�A��A��AO�A�9AffA	�
A��A�FA�hAXA�RAn�A^5Az�AI�A�;A��A��A?}AQ�A �A �@�33@���@�{@��u@�x�@���@��@��+@�A�@��@��@�J@���@�@�-@�O�@��@柾@䛦@��H@�^5@�@�/@�l�@ޟ�@�-@���@�7L@�1'@ڟ�@��@���@���@�dZ@��#@ԋD@�9X@�S�@ҸR@�~�@�E�@�-@�@�?}@�j@�+@Η�@�{@͑h@���@�1'@���@˾w@�l�@��@��@��@�V@Ĭ@�|�@�@�=q@�G�@���@��;@�-@��D@���@��@���@�n�@�~�@�v�@���@��h@�+@��T@�E�@���@�j@���@�r�@� �@�ƨ@��@�b@��F@��@��H@�$�@�$�@�^5@���@�ȴ@��H@���@��+@��\@���@���@��+@���@�5?@�J@�J@�-@��@��@��@�ƨ@�ƨ@�|�@�@�ȴ@��\@�J@��^@���@��h@�p�@�?}@���@��/@��/@��D@�Q�@�b@��@��
@�ƨ@���@�33@�@�ȴ@��R@�-@���@���@�z�@�Z@��@�\)@�+@���@��@�ȴ@��!@�n�@�M�@�$�@��@��h@�X@���@���@��@���@��@��9@��u@���@�1@�dZ@���@�  @�  @��
@���@�dZ@�\)@�\)@�\)@�;d@�;d@�;d@�K�@���@���@�
=@��@�
=@�$�@�7L@���@���@�hs@��@� �@���@��m@�\)@�+@���@��H@��+@�{@�@�@��@���@��j@�Q�@�  @���@�l�@�ƨ@� �@�Z@��@��@�V@�%@��@�z�@��@�  @��
@�C�@���@���@�I�@�Z@�1'@��@�
=@�-@�p�@�?}@��@��/@�z�@�A�@��@��F@�t�@���@�v�@�5?@�@��T@�@���@��@�p�@�hs@�O�@�7L@�%@���@�%@��@���@���@���@��@�j@�Z@�Z@�Z@�A�@��;@�l�@�+@��@��H@���@��!@��\@�~�@�n�@�ff@��R@��\@��+@�^5@��@�5?@�{@�=q@�@��-@���@���@�G�@�&�@�%@��@��@��`@��u@��A@p�?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BĜBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢB�wBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBȴBŢB�)BJB�B/B=qBK�BN�BL�BM�BR�BS�BR�BQ�BK�BD�B9XB,B&�B�BJBB��B�B�BƨB�FB��B��B�BjBJ�B6FBoB
��B
��B
�B
�HB
��B
��B
ȴB
�qB
��B
�uB
n�B
e`B
_;B
YB
E�B
(�B
bB
%B	��B	��B	�B	�mB	�;B	��B	��B	�jB	�-B	�B	��B	��B	�bB	�B	s�B	m�B	jB	gmB	dZB	`BB	YB	R�B	J�B	=qB	49B	1'B	.B	&�B	�B	\B	DB	1B	B	  B��B�B�fB�NB�;B�B��B��B��B��B��B��BȴBȴBŢBB�wB�^B�XB�qB�jB�dB�RB�?B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�oB��B�hB�JB�7B�B�B�B�B�B�1B�7B�JB�VB�hB��B��B��B��B��B�{B�uB�uB��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�3B�LB�RB�RB�dB�jB�qB�}B��B��BBĜBǮB��B��B��B��B��B��B��B��B��B�B�#B�#B�BB�`B�yB�B�B�B��B	  B	+B	PB	uB	�B	�B	%�B	,B	1'B	49B	/B	,B	"�B	%B	
=B	'�B	1'B	2-B	49B	7LB	=qB	@�B	@�B	B�B	B�B	D�B	E�B	G�B	I�B	K�B	L�B	L�B	N�B	P�B	Q�B	S�B	W
B	]/B	bNB	dZB	gmB	iyB	hsB	hsB	hsB	jB	k�B	m�B	o�B	q�B	r�B	t�B	u�B	v�B	w�B	w�B	{�B	}�B	}�B	� B	�B	�%B	�JB	�JB	�JB	�JB	�DB	�JB	�PB	�VB	�bB	�bB	�\B	�VB	�VB	�PB	�VB	�bB	�\B	�\B	�\B	�VB	�VB	�VB	�VB	�\B	�bB	�bB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�LB	�^B	�^B	�qB	�qB	B	ƨB	ǮB	��B	ȴB	ƨB	��B	�}B	�jB	�^B	�dB	�jB	�}B	�}B	��B	��B	ÖB	ĜB	ŢB	ǮB	ǮB	ƨB	ŢB	ŢB	ƨB	ƨB	ɺB	ɺB	��B	��B	��B	��B	�B	�/B	�5B	�;B	�BB	�BB	�HB	�BB	�BB	�;B	�B	�#B	�5B	�HB	�NB	�HB	�BB	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�`B	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
%B
B
B
1B
1B

=B

=B
DB
JB
PB
PB
PB
VB
\B
bB
hB
uB
�B
#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BĜBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢB�wBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBȴBŢB�)BJB�B/B=qBK�BN�BL�BM�BR�BS�BR�BQ�BK�BD�B9XB,B&�B�BJBB��B�B�BƨB�FB��B��B�BjBJ�B6FBoB
��B
��B
�B
�HB
��B
��B
ȴB
�qB
��B
�uB
n�B
e`B
_;B
YB
E�B
(�B
bB
%B	��B	��B	�B	�mB	�;B	��B	��B	�jB	�-B	�B	��B	��B	�bB	�B	s�B	m�B	jB	gmB	dZB	`BB	YB	R�B	J�B	=qB	49B	1'B	.B	&�B	�B	\B	DB	1B	B	  B��B�B�fB�NB�;B�B��B��B��B��B��B��BȴBȴBŢBB�wB�^B�XB�qB�jB�dB�RB�?B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�oB��B�hB�JB�7B�B�B�B�B�B�1B�7B�JB�VB�hB��B��B��B��B��B�{B�uB�uB��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�3B�LB�RB�RB�dB�jB�qB�}B��B��BBĜBǮB��B��B��B��B��B��B��B��B��B�B�#B�#B�BB�`B�yB�B�B�B��B	  B	+B	PB	uB	�B	�B	%�B	,B	1'B	49B	/B	,B	"�B	%B	
=B	'�B	1'B	2-B	49B	7LB	=qB	@�B	@�B	B�B	B�B	D�B	E�B	G�B	I�B	K�B	L�B	L�B	N�B	P�B	Q�B	S�B	W
B	]/B	bNB	dZB	gmB	iyB	hsB	hsB	hsB	jB	k�B	m�B	o�B	q�B	r�B	t�B	u�B	v�B	w�B	w�B	{�B	}�B	}�B	� B	�B	�%B	�JB	�JB	�JB	�JB	�DB	�JB	�PB	�VB	�bB	�bB	�\B	�VB	�VB	�PB	�VB	�bB	�\B	�\B	�\B	�VB	�VB	�VB	�VB	�\B	�bB	�bB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�LB	�^B	�^B	�qB	�qB	B	ƨB	ǮB	��B	ȴB	ƨB	��B	�}B	�jB	�^B	�dB	�jB	�}B	�}B	��B	��B	ÖB	ĜB	ŢB	ǮB	ǮB	ƨB	ŢB	ŢB	ƨB	ƨB	ɺB	ɺB	��B	��B	��B	��B	�B	�/B	�5B	�;B	�BB	�BB	�HB	�BB	�BB	�;B	�B	�#B	�5B	�HB	�NB	�HB	�BB	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�`B	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
%B
B
B
1B
1B

=B

=B
DB
JB
PB
PB
PB
VB
\B
bB
hB
uB
�B
#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140831                              AO  ARCAADJP                                                                    20181024140831    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140831  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140831  QCF$                G�O�G�O�G�O�0               