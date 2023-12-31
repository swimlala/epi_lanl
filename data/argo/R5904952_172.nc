CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:44Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
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
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
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
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190544  20181005190544  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��j$��1   @��j��D�@19������c�� ě�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @&ff@�  @�  A   A   AA��Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B?��BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C��3C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C��3C��3C��3C��3C��3C�  C��C�  C�  C��C��C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� DfD�fD  D� D  D� D��Dy�D  D�fDfD�fD  Dy�D	  D	� D
  D
� D
��D� D  D� D  D�fD  D� D  Dy�D��Dy�D  D� D  D� D  D� D  D�fD  D� DfD� D��D� DfD� D  D�fD  D� D  D� DfD� D  D� DfD�fD  Dy�D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D%��D&� D'fD'�fD(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D,��D-� D.  D.y�D.��D/y�D0  D0�fD1fD1� D2  D2� D3  D3� D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8� D9  D9� D:  D:�fD;fD;� D<  D<�fD=  D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DCy�DD  DD�fDE  DE� DF  DF� DG  DG� DH  DH�fDIfDI�fDJ  DJy�DJ��DK� DL  DLy�DL��DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_fD_� D`  D`� D`��Da� Db  Db� DcfDc� Dd  Dd�fDe  Dey�Df  Df�fDg  Dg� Dh  Dhy�Di  Di�fDj  Dj� Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dn� Dn��Doy�Do��Dpy�Dq  Dq� Dr  Dr� Dr��Dsy�Ds��Dty�Du  Du�fDvfDv� Dw  Dw�fDw��Dy�fD�D�D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @2�\@�{@�{A
=A#
=AD��Ad��A��A��A��A��A��AхA�A�B B\)BBB B(B0B8B@\)BHBPBXBa(�Bi(�BpBxB�aHB�aHB�.B�aHB�aHB�aHB��{B�aHB�aHB�aHB��{B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC 0�CJ>CJ>CJ>C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"J>C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct
Cv0�Cx0�Cz
C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC��C��C�RC�RC�RC�RC�RC��C��C��C��C�RC�%C�%C�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�%C�%C�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC��C��C��C��C�RC�RC�RC��C�RC�%C�%C�RC�RC��C�RC�RC�RC�RC�RC�RC�RC��C�RC�RC��C�RC�RC��C�RC�RC��C��C��C��C��C�RC�%C�RC�RC�%C�%C�RC��C��C�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RD �D �)D)D�)D�D��D)D�)D)D�)D�D��D)D��D�D��D)D��D	)D	�)D
)D
�)D�D�)D)D�)D)D��D)D�)D)D��D�D��D)D�)D)D�)D)D�)D)D��D)D�)D�D�)D�D�)D�D�)D)D��D)D�)D)D�)D�D�)D)D�)D�D��D)D��D �D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%��D&�D&�)D'�D'��D(�D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-�D-�)D.)D.��D/�D/��D0)D0��D1�D1�)D2)D2�)D3)D3�)D4�D4��D5�D5��D6�D6��D7�D7��D8�D8�)D9)D9�)D:)D:��D;�D;�)D<)D<��D=)D=��D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC�DC��DD)DD��DE)DE�)DF)DF�)DG)DG�)DH)DH��DI�DI��DJ)DJ��DK�DK�)DL)DL��DM�DM�)DN)DN�)DO)DO�)DP)DP��DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV��DW)DW�)DX�DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]��D^)D^�)D_�D_�)D`)D`�)Da�Da�)Db)Db�)Dc�Dc�)Dd)Dd��De)De��Df)Df��Dg)Dg�)Dh)Dh��Di)Di��Dj)Dj�)Dk)Dk�)Dl�Dl�)Dm)Dm�)Dn)Dn�)Do�Do��Dp�Dp��Dq)Dq�)Dr)Dr�)Ds�Ds��Dt�Dt��Du)Du��Dv�Dv�)Dw)Dw��Dw��Dy��D�J�D��z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A��
A���A��
A��
A��
A��#A��;A��HA��HA��HA��TA��TA��mA��yA��A�  A�JA�/Aϥ�A�oA�VAжFAмjAа!AЏ\AЗ�AУ�A�XA��A���Aϗ�A�A�A��A�Aκ^A�JA�O�A���A�5?A���A��#A���A˝�A�bNAȺ^AƏ\Aũ�A�A�^5A�^5A�A���A��A��;A��A���A��/A���A�G�A��A�A��
A�dZA�jA�VA�33A�1'A��-A��mA�?}A�5?A�O�A���A�(�A���A��A��PA�?}A�"�A�
=A��DA��FA�G�A�ĜA��/A���A�M�A��A�I�A���A�x�A���A�%A�E�A�^5A��yA��#A���A���A��\A��TA��A�7LA�z�A�G�A�I�A��mA�dZA�ƨA{`BAy��At�/AoG�Al��Ah��Ac��A^VAZr�AV�AR��AR �AQ�PAQ�AMXAJM�AG�PAE&�ACXAAhsA?A?�A?�A?��A?oA="�A<M�A;\)A:��A9oA6n�A5VA4ffA2�jA1+A/�A/�PA-�;A,5?A+t�A*-A'��A%�^A%�A$��A#hsA!��A|�A�At�A�9A�^AS�A��AK�AAM�A�#AC�A�mA�9A9XA��A��A��A`BA�HA\)A
�\A
bA	ƨA�yAffAƨA
=A�AȴAjA$�A��A�RA^5AƨA`BA33A ��A ��A �@���@�Z@�^5@�%@�9X@�;d@�I�@�C�@���@�dZ@�+@�X@�j@�I�@��@��H@�@�Ĝ@�
=@��@�@�1'@�w@�+@�^@�|�@�J@ܬ@۝�@���@�M�@�-@���@�x�@�G�@���@�Ĝ@؛�@�|�@�5?@�G�@���@�b@�ff@Ѓ@�dZ@��@�ȴ@�~�@�E�@���@͙�@�O�@���@˥�@�+@�v�@���@�V@�z�@��@�S�@Ƈ+@�5?@ź^@š�@ŉ7@Ĵ9@�j@�9X@Õ�@��@�V@���@��7@�?}@��@�ƨ@���@�o@�~�@��^@�?}@��@��@���@�I�@�1@���@�K�@�~�@��7@�O�@�/@�O�@��`@��@�bN@�1@��y@�5?@��@���@�hs@���@�r�@� �@���@�o@��@��H@�ff@�$�@�$�@��@��#@�p�@�?}@��`@��j@���@�A�@���@�t�@�ȴ@��\@�5?@�@�/@���@��9@�Z@��
@���@�o@���@��\@�ff@�=q@���@��7@�O�@�G�@�?}@��@�A�@�  @��;@�ƨ@��w@��@�C�@�
=@��H@�E�@��@�7L@�(�@��@���@��@��P@�|�@�dZ@���@��-@�O�@�G�@�7L@��j@���@��9@�bN@�  @���@���@��@�K�@��H@�V@���@��@�?}@�V@�Ĝ@�bN@�(�@�b@���@��F@�|�@�l�@�
=@���@�^5@��@���@���@���@��7@�p�@�7L@��@�V@��/@��u@�j@�I�@�b@��@�\)@�dZ@�\)@�;d@��@�o@�ȴ@��R@��R@���@���@�"�@�
=@��H@���@�ff@�V@��@���@��@�V@��`@��@�9X@�1@��
@�|�@�K�@�@�ȴ@�~�@�J@��@��#@���@��^@���@��@�X@�G�@�&�@���@�j@�9X@�1@��;@���@��P@��P@�t�@�;d@��@��y@��!@��+@�-@��^@�p�@�?}@�V@��`@��@�z�@�j@�I�@�1@���@��;@��F@���@�S�@�;d@���@���@��+@�@��^@��7@�O�@��@���@�bN@��@�a�@S�@j�x1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A���A���A��
A���A��
A��
A��
A��#A��;A��HA��HA��HA��TA��TA��mA��yA��A�  A�JA�/Aϥ�A�oA�VAжFAмjAа!AЏ\AЗ�AУ�A�XA��A���Aϗ�A�A�A��A�Aκ^A�JA�O�A���A�5?A���A��#A���A˝�A�bNAȺ^AƏ\Aũ�A�A�^5A�^5A�A���A��A��;A��A���A��/A���A�G�A��A�A��
A�dZA�jA�VA�33A�1'A��-A��mA�?}A�5?A�O�A���A�(�A���A��A��PA�?}A�"�A�
=A��DA��FA�G�A�ĜA��/A���A�M�A��A�I�A���A�x�A���A�%A�E�A�^5A��yA��#A���A���A��\A��TA��A�7LA�z�A�G�A�I�A��mA�dZA�ƨA{`BAy��At�/AoG�Al��Ah��Ac��A^VAZr�AV�AR��AR �AQ�PAQ�AMXAJM�AG�PAE&�ACXAAhsA?A?�A?�A?��A?oA="�A<M�A;\)A:��A9oA6n�A5VA4ffA2�jA1+A/�A/�PA-�;A,5?A+t�A*-A'��A%�^A%�A$��A#hsA!��A|�A�At�A�9A�^AS�A��AK�AAM�A�#AC�A�mA�9A9XA��A��A��A`BA�HA\)A
�\A
bA	ƨA�yAffAƨA
=A�AȴAjA$�A��A�RA^5AƨA`BA33A ��A ��A �@���@�Z@�^5@�%@�9X@�;d@�I�@�C�@���@�dZ@�+@�X@�j@�I�@��@��H@�@�Ĝ@�
=@��@�@�1'@�w@�+@�^@�|�@�J@ܬ@۝�@���@�M�@�-@���@�x�@�G�@���@�Ĝ@؛�@�|�@�5?@�G�@���@�b@�ff@Ѓ@�dZ@��@�ȴ@�~�@�E�@���@͙�@�O�@���@˥�@�+@�v�@���@�V@�z�@��@�S�@Ƈ+@�5?@ź^@š�@ŉ7@Ĵ9@�j@�9X@Õ�@��@�V@���@��7@�?}@��@�ƨ@���@�o@�~�@��^@�?}@��@��@���@�I�@�1@���@�K�@�~�@��7@�O�@�/@�O�@��`@��@�bN@�1@��y@�5?@��@���@�hs@���@�r�@� �@���@�o@��@��H@�ff@�$�@�$�@��@��#@�p�@�?}@��`@��j@���@�A�@���@�t�@�ȴ@��\@�5?@�@�/@���@��9@�Z@��
@���@�o@���@��\@�ff@�=q@���@��7@�O�@�G�@�?}@��@�A�@�  @��;@�ƨ@��w@��@�C�@�
=@��H@�E�@��@�7L@�(�@��@���@��@��P@�|�@�dZ@���@��-@�O�@�G�@�7L@��j@���@��9@�bN@�  @���@���@��@�K�@��H@�V@���@��@�?}@�V@�Ĝ@�bN@�(�@�b@���@��F@�|�@�l�@�
=@���@�^5@��@���@���@���@��7@�p�@�7L@��@�V@��/@��u@�j@�I�@�b@��@�\)@�dZ@�\)@�;d@��@�o@�ȴ@��R@��R@���@���@�"�@�
=@��H@���@�ff@�V@��@���@��@�V@��`@��@�9X@�1@��
@�|�@�K�@�@�ȴ@�~�@�J@��@��#@���@��^@���@��@�X@�G�@�&�@���@�j@�9X@�1@��;@���@��P@��P@�t�@�;d@��@��y@��!@��+@�-@��^@�p�@�?}@�V@��`@��@�z�@�j@�I�@�1@���@��;@��F@���@�S�@�;d@���@���@��+@�@��^@��7@�O�@��@���@�bN@��@�a�@S�@j�x1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bs�Bs�Bs�Bs�Bt�Bu�Bu�Bt�Bu�Bt�Bt�Bt�Bt�Bu�Bt�Bt�Bt�Bt�Bu�Bv�B{�B�JB��B�XB	I�B
PB
v�B{B"�B%�B2-B9XB]/Bk�Br�Bv�B�B�7B�7B�+B� Bq�B_;BQ�BA�B;dB8RB?}BE�BaHB�{B�'BÖB��B�
B�mB�BB#�B,B9XBI�BP�BQ�BQ�BQ�BQ�BT�B\)B`BBhsBl�BjBgmB[#BM�BE�B>wB9XB33B0!B,B �B�B
=B�B�
B��BǮB��B�FB��B��B�Bs�BiyBP�B49B�B
��B
�B
�HB
��B
�^B
��B
��B
�\B
{�B
hsB
R�B
@�B
1'B
�B
%B	�B	��B	�XB	��B	jB	ZB	XB	A�B	�B�B�)B�B�B��B��BB�-B��B��B�B�LB�XB�XB�XB�RB�XB�wB�qB�jB�XB�FB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�-B�RB�^B�dB�qB�jB�^B�XB�XB�XB�dB�jB�}BĜBĜBǮBŢBÖBǮB��B��B��B��B��B�#B�HB�BB�;B�/B�B�B��B��B��B��B��B�B�B�)B�)B�#B�#B�)B�HB�TB�ZB�ZB�fB�mB�mB�B�B�B�B��B��B��B��B	B	B	+B	1B	1B	bB	{B	�B	�B	{B	oB	�B	�B	�B	�B	 �B	 �B	%�B	&�B	%�B	%�B	(�B	+B	.B	1'B	33B	6FB	9XB	?}B	C�B	D�B	H�B	K�B	L�B	O�B	R�B	R�B	XB	[#B	_;B	cTB	dZB	e`B	iyB	l�B	n�B	q�B	s�B	v�B	y�B	y�B	y�B	{�B	~�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�VB	�\B	�bB	�bB	�hB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�3B	�?B	�FB	�FB	�LB	�RB	�wB	��B	��B	��B	B	ÖB	ĜB	ƨB	ɺB	��B	ɺB	ǮB	ÖB	ÖB	ĜB	ŢB	ŢB	ǮB	ȴB	ǮB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�HB	�NB	�NB	�TB	�TB	�ZB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B
	7B

=B

=B
DB
PB
PB
%`B
./2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 Bs�Bs�Bs�Bs�Bt�Bu�Bu�Bt�Bu�Bt�Bt�Bt�Bt�Bu�Bt�Bt�Bt�Bt�Bu�Bv�B{�B�JB��B�XB	I�B
PB
v�B{B"�B%�B2-B9XB]/Bk�Br�Bv�B�B�7B�7B�+B� Bq�B_;BQ�BA�B;dB8RB?}BE�BaHB�{B�'BÖB��B�
B�mB�BB#�B,B9XBI�BP�BQ�BQ�BQ�BQ�BT�B\)B`BBhsBl�BjBgmB[#BM�BE�B>wB9XB33B0!B,B �B�B
=B�B�
B��BǮB��B�FB��B��B�Bs�BiyBP�B49B�B
��B
�B
�HB
��B
�^B
��B
��B
�\B
{�B
hsB
R�B
@�B
1'B
�B
%B	�B	��B	�XB	��B	jB	ZB	XB	A�B	�B�B�)B�B�B��B��BB�-B��B��B�B�LB�XB�XB�XB�RB�XB�wB�qB�jB�XB�FB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�-B�RB�^B�dB�qB�jB�^B�XB�XB�XB�dB�jB�}BĜBĜBǮBŢBÖBǮB��B��B��B��B��B�#B�HB�BB�;B�/B�B�B��B��B��B��B��B�B�B�)B�)B�#B�#B�)B�HB�TB�ZB�ZB�fB�mB�mB�B�B�B�B��B��B��B��B	B	B	+B	1B	1B	bB	{B	�B	�B	{B	oB	�B	�B	�B	�B	 �B	 �B	%�B	&�B	%�B	%�B	(�B	+B	.B	1'B	33B	6FB	9XB	?}B	C�B	D�B	H�B	K�B	L�B	O�B	R�B	R�B	XB	[#B	_;B	cTB	dZB	e`B	iyB	l�B	n�B	q�B	s�B	v�B	y�B	y�B	y�B	{�B	~�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�VB	�\B	�bB	�bB	�hB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�3B	�?B	�FB	�FB	�LB	�RB	�wB	��B	��B	��B	B	ÖB	ĜB	ƨB	ɺB	��B	ɺB	ǮB	ÖB	ÖB	ĜB	ŢB	ŢB	ǮB	ȴB	ǮB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�HB	�NB	�NB	�TB	�TB	�ZB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B
	7B

=B

=B
DB
PB
PB
%`B
./2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.19 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190544                              AO  ARCAADJP                                                                    20181005190544    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190544  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190544  QCF$                G�O�G�O�G�O�8000            