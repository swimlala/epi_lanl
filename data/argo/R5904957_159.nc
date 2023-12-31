CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:34Z creation      
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
resolution        =���   axis      Z        d  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ST   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  m8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  vx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140834  20181024140834  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��dڤ�p1   @��ehK��@5����o�c���v�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   B   B   @�33@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  BffB ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CE�fCG�fCJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  Dy�D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4y�D5  D5� D6  D6� D7  D7� D8  D8� D9fD9�fD:  D:y�D;  D;� D<  D<� D=  D=�fD>  D>y�D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� DdfDd� De  Dey�De��Df� Dg  Dg�fDhfDh�fDifDi�fDjfDj�fDk  Dky�Dk��Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dyt{D�7
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=p@�
=A�A#�AC�Ac�A�A���A�A�A�A�A�A�B �HB�HB�HBG�B!G�B(�HB0�HB8�HB@�HBH�HBP�HBX�HB`�HBh�HBp�HBx�HB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�C 8RC8RC8RC8RC8RC
�C8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC Q�C"8RC$8RC&8RC(8RC*8RC,8RC.Q�C08RC28RC48RC68RC88RC:�C<8RC>8RC@8RCB8RCD8RCF�CH�CJ8RCLQ�CN8RCP8RCR8RCT8RCV8RCX8RCZ8RC\Q�C^8RC`8RCb8RCd8RCf8RChQ�Cj8RCl8RCn8RCp8RCr8RCt8RCv8RCx8RCz8RC|8RC~�C�)C�)C�)C�)C�)C�)C�\C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�(�C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)D D �DD��DD�DD�DD�DD�DzD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DzD�D�D�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4��D5D5�D6D6�D7D7�D8D8�D9zD9�zD:D:��D;D;�D<D<�D=D=�zD>D>��D?D?�D@D@�DADA�DBDB��DCDC�DDDD�DEDE�DFDF�zDGDG�DHDH��DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVzDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbzDb�DcDc�DdzDd�DeDe��Df�Df�DgDg�zDhzDh�zDizDi�zDjzDj�zDkDk��Dl�Dl�DmDm�Dn�Dn�DoDo�DpDp�DqDq�DrDr�DsDs�zDtDt�DuDu�DvDv�DwDw�Dy��D�>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�A�A�C�A�G�A�G�A�C�A�C�A�E�A�S�A�^5A�bNA�v�A�~�Aԟ�Aԏ\A�`BA�A�A�=qA�9XA�1'A�z�A�`BA�`BA�1A�ȴA�ĜA���A�G�AÍPA���A�p�A�&�A��TA��A���A��+A��A�+A��TA���A��A���A��A��wA�5?A�  A��7A�I�A���A���A���A��A��;A�ƨA���A�M�A�\)A���A�$�A�33A��A��uA�1'A�bA��A��A�`BA�ƨA��#A��A��mA�ffA��A��^A��+A�=qA��A��A�M�A���A�1'A�p�A��A�33A�z�A�/A�hsA���A��RA�^5A�t�A���A���A���A��\A��hAt�A�A|�9Ax5?Au�Aq�7Am�Ai�Ah��Ag�Af{Ad^5Ab��AaS�A`(�A^�jAZffAW�PAV�AV$�AUp�AT  AP=qAO��AL��AJ��AIAI\)AH�AHI�AF1'AEAC��AB�+AA��A?�
A>$�A;XA:�A:  A9x�A8I�A7O�A6�yA6z�A5&�A3O�A09XA.jA$=qA"��A!`BA�TA�-A��A/A�`AĜAr�A�A��A�RA��A�wA/A
=A�`AbNA��A�/A��A�\A��A�HA�RA�mA�A=qA|�A�A
ĜA
bA	��A�\AdZAr�AdZAA�PA A�A 1'A   @��
@���@���@���@��@�@��w@��+@�P@�M�@� �@�ff@�@���@�M�@��@�1@�\@��@�^@�`B@�G�@�j@䛦@�Z@�ȴ@�-@߮@݉7@�j@�C�@�ȴ@�=q@���@��@�r�@�G�@��@��@�/@��m@�^5@ͩ�@́@�?}@��@˅@ʸR@ʏ\@�=q@�@�/@�Q�@�E�@�b@�dZ@�o@�$�@�1@��m@��m@���@���@��-@��-@���@���@��j@��u@�A�@��P@��H@�-@�@��@���@�r�@��m@�l�@��y@�V@�@���@��/@��@��D@�j@�A�@�(�@�1@���@��m@��
@��@�t�@�dZ@�
=@��H@��@��@��\@��^@���@��@��
@���@�S�@��H@��\@��@���@��@�9X@���@���@��@�Q�@�9X@�  @���@�;d@�
=@�{@�x�@�x�@���@���@���@�bN@�r�@���@�;d@�33@�=q@�{@��@���@��h@�&�@��@���@�Ĝ@��@��u@�r�@�bN@�Q�@�Q�@�A�@�1'@�b@��@���@��
@��w@�|�@�"�@��@�ȴ@���@�v�@�M�@�@��j@�z�@�ƨ@�|�@�;d@���@�M�@�M�@�E�@�J@�x�@�?}@���@��9@��D@�r�@�Q�@�9X@�  @��w@��@��@��@���@�|�@�K�@�;d@�;d@�o@��@���@�@���@��y@�ȴ@���@��R@�ȴ@��H@��H@���@���@��-@��@�hs@�V@�I�@��@���@�|�@�t�@�t�@�\)@�"�@�@�ȴ@���@��+@�^5@�-@��@��-@��@��9@���@�1'@��m@�ƨ@���@��P@�dZ@�;d@��H@��R@��+@�ff@�V@�V@�=q@��#@�@���@�x�@�7L@��@��@��@���@��@���@���@���@��u@�1'@�ƨ@�K�@�@��R@���@���@���@���@���@���@���@��\@�v�@�ff@�V@�M�@�5?@��@���@��h@�hs@�?}@�/@�&�@��@��@�ی@~�'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�A�A�A�C�A�G�A�G�A�C�A�C�A�E�A�S�A�^5A�bNA�v�A�~�Aԟ�Aԏ\A�`BA�A�A�=qA�9XA�1'A�z�A�`BA�`BA�1A�ȴA�ĜA���A�G�AÍPA���A�p�A�&�A��TA��A���A��+A��A�+A��TA���A��A���A��A��wA�5?A�  A��7A�I�A���A���A���A��A��;A�ƨA���A�M�A�\)A���A�$�A�33A��A��uA�1'A�bA��A��A�`BA�ƨA��#A��A��mA�ffA��A��^A��+A�=qA��A��A�M�A���A�1'A�p�A��A�33A�z�A�/A�hsA���A��RA�^5A�t�A���A���A���A��\A��hAt�A�A|�9Ax5?Au�Aq�7Am�Ai�Ah��Ag�Af{Ad^5Ab��AaS�A`(�A^�jAZffAW�PAV�AV$�AUp�AT  AP=qAO��AL��AJ��AIAI\)AH�AHI�AF1'AEAC��AB�+AA��A?�
A>$�A;XA:�A:  A9x�A8I�A7O�A6�yA6z�A5&�A3O�A09XA.jA$=qA"��A!`BA�TA�-A��A/A�`AĜAr�A�A��A�RA��A�wA/A
=A�`AbNA��A�/A��A�\A��A�HA�RA�mA�A=qA|�A�A
ĜA
bA	��A�\AdZAr�AdZAA�PA A�A 1'A   @��
@���@���@���@��@�@��w@��+@�P@�M�@� �@�ff@�@���@�M�@��@�1@�\@��@�^@�`B@�G�@�j@䛦@�Z@�ȴ@�-@߮@݉7@�j@�C�@�ȴ@�=q@���@��@�r�@�G�@��@��@�/@��m@�^5@ͩ�@́@�?}@��@˅@ʸR@ʏ\@�=q@�@�/@�Q�@�E�@�b@�dZ@�o@�$�@�1@��m@��m@���@���@��-@��-@���@���@��j@��u@�A�@��P@��H@�-@�@��@���@�r�@��m@�l�@��y@�V@�@���@��/@��@��D@�j@�A�@�(�@�1@���@��m@��
@��@�t�@�dZ@�
=@��H@��@��@��\@��^@���@��@��
@���@�S�@��H@��\@��@���@��@�9X@���@���@��@�Q�@�9X@�  @���@�;d@�
=@�{@�x�@�x�@���@���@���@�bN@�r�@���@�;d@�33@�=q@�{@��@���@��h@�&�@��@���@�Ĝ@��@��u@�r�@�bN@�Q�@�Q�@�A�@�1'@�b@��@���@��
@��w@�|�@�"�@��@�ȴ@���@�v�@�M�@�@��j@�z�@�ƨ@�|�@�;d@���@�M�@�M�@�E�@�J@�x�@�?}@���@��9@��D@�r�@�Q�@�9X@�  @��w@��@��@��@���@�|�@�K�@�;d@�;d@�o@��@���@�@���@��y@�ȴ@���@��R@�ȴ@��H@��H@���@���@��-@��@�hs@�V@�I�@��@���@�|�@�t�@�t�@�\)@�"�@�@�ȴ@���@��+@�^5@�-@��@��-@��@��9@���@�1'@��m@�ƨ@���@��P@�dZ@�;d@��H@��R@��+@�ff@�V@�V@�=q@��#@�@���@�x�@�7L@��@��@��@���@��@���@���@���@��u@�1'@�ƨ@�K�@�@��R@���@���@���@���@���@���@���@��\@�v�@�ff@�V@�M�@�5?@��@���@��h@�hs@�?}@�/@�&�@��@��@�ی@~�'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�dB
�dB
�^B
�^B
�^B
�^B
�^B
�dB
�jB
�}B
B
�)B
�BL�B�7B�mB+B1B1B1BBBBBuB �B �B-B49B?}B@�B@�BE�BH�BE�BK�BN�BN�BN�BL�BQ�BS�BS�BS�BQ�BQ�BQ�BP�BP�BP�BP�BP�BO�BR�BXB[#BYBR�BN�BC�B.B �B�B\B	7BB��B�BŢB�XB��B��B��B��B�hB�JB{�BaHBXBL�B:^B)�B�BB
��B
�B
�`B
�#B
��B
�}B
�'B
��B
z�B
bNB
W
B
J�B
>wB
:^B
#�B
B	��B	�ZB	ɺB	�FB	�B	��B	��B	�oB	�7B	~�B	u�B	jB	Q�B	A�B	>wB	:^B	6FB	.B	�B	�B	bB	1B	B	B	  B��B��B�B�B�fB�NB�)B��B��B��B��BɺBǮBŢBĜBB�qB�RB�9BZB��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�\B�\B�hB�oB�oB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�?B�LB�RB�XB�XB�RB�RB�^B�dB�}B��BŢB��B��B��B��B��B��B�B�B�B�#B�/B�;B�fB�B�B�B�B��B��B��B��B	B	+B	+B	+B		7B	
=B	
=B	JB	VB	hB	{B	�B	�B	�B	�B	!�B	%�B	+B	.B	1'B	49B	;dB	<jB	=qB	=qB	>wB	?}B	@�B	A�B	B�B	C�B	G�B	M�B	R�B	W
B	YB	[#B	]/B	`BB	iyB	q�B	t�B	u�B	v�B	w�B	x�B	y�B	{�B	{�B	{�B	~�B	�%B	�DB	�VB	�\B	�\B	�bB	�oB	�{B	�{B	�{B	�uB	�oB	�hB	�bB	�bB	�\B	�\B	�VB	�VB	�PB	�VB	�\B	�\B	�\B	�\B	�hB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�?B	�?B	�LB	�jB	�qB	�wB	�}B	�}B	��B	��B	��B	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�5B	�BB	�HB	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
%B
%B
%B
%B
+B
1B
	7B

=B

=B

=B
DB
DB

�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�dB
�dB
�^B
�^B
�^B
�^B
�^B
�dB
�jB
�}B
B
�)B
�BL�B�7B�mB+B1B1B1BBBBBuB �B �B-B49B?}B@�B@�BE�BH�BE�BK�BN�BN�BN�BL�BQ�BS�BS�BS�BQ�BQ�BQ�BP�BP�BP�BP�BP�BO�BR�BXB[#BYBR�BN�BC�B.B �B�B\B	7BB��B�BŢB�XB��B��B��B��B�hB�JB{�BaHBXBL�B:^B)�B�BB
��B
�B
�`B
�#B
��B
�}B
�'B
��B
z�B
bNB
W
B
J�B
>wB
:^B
#�B
B	��B	�ZB	ɺB	�FB	�B	��B	��B	�oB	�7B	~�B	u�B	jB	Q�B	A�B	>wB	:^B	6FB	.B	�B	�B	bB	1B	B	B	  B��B��B�B�B�fB�NB�)B��B��B��B��BɺBǮBŢBĜBB�qB�RB�9BZB��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�\B�\B�hB�oB�oB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�?B�LB�RB�XB�XB�RB�RB�^B�dB�}B��BŢB��B��B��B��B��B��B�B�B�B�#B�/B�;B�fB�B�B�B�B��B��B��B��B	B	+B	+B	+B		7B	
=B	
=B	JB	VB	hB	{B	�B	�B	�B	�B	!�B	%�B	+B	.B	1'B	49B	;dB	<jB	=qB	=qB	>wB	?}B	@�B	A�B	B�B	C�B	G�B	M�B	R�B	W
B	YB	[#B	]/B	`BB	iyB	q�B	t�B	u�B	v�B	w�B	x�B	y�B	{�B	{�B	{�B	~�B	�%B	�DB	�VB	�\B	�\B	�bB	�oB	�{B	�{B	�{B	�uB	�oB	�hB	�bB	�bB	�\B	�\B	�VB	�VB	�PB	�VB	�\B	�\B	�\B	�\B	�hB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�?B	�?B	�LB	�jB	�qB	�wB	�}B	�}B	��B	��B	��B	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�5B	�BB	�HB	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
%B
%B
%B
%B
+B
1B
	7B

=B

=B

=B
DB
DB

�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140834                              AO  ARCAADJP                                                                    20181024140834    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140834  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140834  QCF$                G�O�G�O�G�O�4000            