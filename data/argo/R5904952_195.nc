CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:50Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190550  20181005190550  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��*	@1   @��*�8��@1��O�;d�c��t�j1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  @���A   A@  A`  A�  A�  A���A���A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCO�fCR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd�Cf�Ch�Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C��C��C��C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3D � DfD� D��Dy�D  D� D  D�fD  D� D  Dy�D  D� D  Dy�D	  D	� D
  D
y�D
��D� DfD� D  D� D  D� D  D� D��D� D  D� D��D� D  D� DfD�fD  D� DfD� D  D�fD  D� D��D� DfD� D��D� D  D� D  D� D��D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*y�D+  D+�fD,  D,� D-  D-�fD.  D.� D/  D/� D0  D0� D1  D1�fD2fD2� D3  D3� D4  D4� D4��D5y�D5��D6y�D7  D7� D8  D8� D9  D9� D:  D:�fD;fD;�fD<  D<� D<��D=y�D>  D>� D?fD?�fD@  D@y�D@��DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK�fDL  DL� DM  DM�fDN  DNy�DO  DO� DP  DP� DQ  DQ� DQ��DRy�DR��DSy�DT  DT�fDU  DU� DV  DV� DV��DWy�DW��DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dd  Dd� De  De� Df  Df� DgfDg� Dg��Dh� Di  Di�fDj  Djy�Dk  Dk�fDl  Dl� Dm  Dm�fDnfDn� Dn��Do� Dp  Dp� Dq  Dq�fDr  Dry�Ds  Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dw� DwٚDy�fD�?�D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@�{Ap�A#
=AC
=Ac
=A��A��A�Q�A�Q�A��AхA�A�Q�B BBBB B(B0B8B@BHBPBXB`BhBp\)BxB�aHB�aHB�aHB�aHB�aHB�aHB�aHB��{B�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�.B�aHB̔{BД{B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C0J>C2J>C40�C60�C80�C:
C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN
CP
CR0�CT
CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�CdJ>CfJ>ChJ>Cj0�ClJ>Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC��C�RC�%C�%C�%C�RC��C�RC�RC�RC�RC�RC�%C�%C�RC�RC�RC�%C�RC�RC�%C�RC��C��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�%C�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC��C��C�RC�RC�RC�RC�RD �D �)D�D�)D�D��D)D�)D)D��D)D�)D)D��D)D�)D)D��D	)D	�)D
)D
��D�D�)D�D�)D)D�)D)D�)D)D�)D�D�)D)D�)D�D�)D)D�)D�D��D)D�)D�D�)D)D��D)D�)D�D�)D�D�)D�D�)D)D�)D)D�)D�D�)D)D�)D �D �)D!)D!�)D")D"�)D#)D#�)D$�D$�)D%)D%�)D&)D&�)D')D'�)D(�D(�)D))D)�)D*)D*��D+)D+��D,)D,�)D-)D-��D.)D.�)D/)D/�)D0)D0�)D1)D1��D2�D2�)D3)D3�)D4)D4�)D5�D5��D6�D6��D7)D7�)D8)D8�)D9)D9�)D:)D:��D;�D;��D<)D<�)D=�D=��D>)D>�)D?�D?��D@)D@��DA�DA�)DB�DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK�DK��DL)DL�)DM)DM��DN)DN��DO)DO�)DP)DP�)DQ)DQ�)DR�DR��DS�DS��DT)DT��DU)DU�)DV)DV�)DW�DW��DX�DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^�D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc��Dd)Dd�)De)De�)Df)Df�)Dg�Dg�)Dh�Dh�)Di)Di��Dj)Dj��Dk)Dk��Dl)Dl�)Dm)Dm��Dn�Dn�)Do�Do�)Dp)Dp�)Dq)Dq��Dr)Dr��Ds)Ds�)Dt)Dt�)Du)Du��Dv)Dv�)Dw)Dw�)Dw��Dy��D�E�D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�G�A�G�A�I�A�I�A�K�A�Q�A�Q�A�33A�ĜAƺ^AƩ�Aƕ�AƅA�z�A�jA�/A���A���Aš�Aŉ7A�jA�VA�XA�dZA�p�A�|�A�~�A�XA�{A��AľwA���A���Aė�AăAĝ�A�A�A�33A�{AļjA��Aç�Aá�AËDA�9XA��`A�ȴA�r�A�K�A�/A�A�XA�~�A�7LA��`A��A�^5A��HA�`BA��yA�
=A��
A�$�A��A�A�A�ȴA��-A���A�=qA�$�A���A�x�A�E�A���A�"�A�A�A���A���A���A�A�A�\)A��A�1A�bA��;A�v�A�ĜA�G�A�I�A��A��yA��A�7LA��TA��HA��!A�E�A�  A�(�A�hsA��A� �A�bNA�K�A~�A|�yAz��At�!AqdZAj��Ah~�AfbNAaO�A]��A\�uA\z�A\~�A\v�A\=qA[K�AY��AW��ATA�AOhsAK��AGAG33AF9XAE"�AA?}A>I�A=�A=�A=�A: �A7�^A6��A5�wA3?}A1�A0�A/�A-�wA*E�A(�A(1'A&�yA$�yA$ZA"�A!�A E�A�HA�A�mAt�A��Ap�Av�Az�A��A%A�;A?}A�FA7LAoA�A��A��AO�AȴA�A��A�A��A�A=qAt�A
��A
=qA	��A�A5?A��A��A/A�A�AȴA~�A��A��A�/A�DA;dA��A ��A j@��@�M�@��;@�v�@�p�@��H@��u@�hs@�t�@�/@��@�|�@���@�x�@�?}@�u@�b@�@�l�@�v�@��@�/@�Ĝ@�A�@��
@�^5@�p�@�/@��/@�z�@�b@�K�@ޏ\@�`B@ܛ�@ۮ@��H@��@���@��@ץ�@ם�@�33@��@�O�@ԓu@�|�@ҧ�@�-@�hs@��@д9@�;d@�{@͙�@���@̴9@̬@̴9@̴9@̬@̓u@��y@�v�@��@ɉ7@�p�@�G�@���@ȋD@�j@�(�@��@� �@�Z@�Z@� �@��@���@���@�Z@�j@�z�@�j@�A�@��@��
@��H@�33@ƸR@š�@���@�z�@��
@�v�@���@�z�@���@��@�b@��@��@� �@�  @���@�+@�@�33@�33@�;d@�l�@�dZ@�33@�~�@��@��@�@�X@���@�@��@��9@�9X@���@�C�@�@�v�@�$�@�{@��h@�hs@�O�@�O�@��`@���@��m@��@�@�~�@��@��@���@�x�@���@�1'@� �@�1'@��;@�=q@�G�@�V@�V@�V@��@��@�1@��@��@�"�@�@��\@�M�@��@�@��#@���@���@�@���@�?}@�bN@�1@���@�;d@��H@���@��R@�~�@�V@�=q@�@���@�$�@�-@�5?@�{@�{@��@��h@���@�G�@�%@�Ĝ@�z�@�Q�@�I�@�  @���@�+@�@��@���@�@�`B@��D@��;@���@�t�@��@���@�M�@�@���@�Z@���@��P@�
=@���@�M�@��@��@���@��-@�x�@�&�@���@�bN@�1@��@�S�@�
=@���@�ff@�E�@�J@���@�?}@�%@��`@���@���@�j@�A�@�1@���@���@�dZ@�
=@��y@���@�ȴ@��R@���@�E�@���@�@���@���@���@���@�p�@�`B@�`B@�`B@�`B@�`B@�X@�O�@�V@���@�Ĝ@��j@�Q�@��@���@��@���@���@���@��\@��+@�n�@�^5@��@���@���@�X@��@���@�b@�  @��@��w@�C�@��!@�ԕ@�v`@t~(111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�G�A�G�A�I�A�I�A�K�A�Q�A�Q�A�33A�ĜAƺ^AƩ�Aƕ�AƅA�z�A�jA�/A���A���Aš�Aŉ7A�jA�VA�XA�dZA�p�A�|�A�~�A�XA�{A��AľwA���A���Aė�AăAĝ�A�A�A�33A�{AļjA��Aç�Aá�AËDA�9XA��`A�ȴA�r�A�K�A�/A�A�XA�~�A�7LA��`A��A�^5A��HA�`BA��yA�
=A��
A�$�A��A�A�A�ȴA��-A���A�=qA�$�A���A�x�A�E�A���A�"�A�A�A���A���A���A�A�A�\)A��A�1A�bA��;A�v�A�ĜA�G�A�I�A��A��yA��A�7LA��TA��HA��!A�E�A�  A�(�A�hsA��A� �A�bNA�K�A~�A|�yAz��At�!AqdZAj��Ah~�AfbNAaO�A]��A\�uA\z�A\~�A\v�A\=qA[K�AY��AW��ATA�AOhsAK��AGAG33AF9XAE"�AA?}A>I�A=�A=�A=�A: �A7�^A6��A5�wA3?}A1�A0�A/�A-�wA*E�A(�A(1'A&�yA$�yA$ZA"�A!�A E�A�HA�A�mAt�A��Ap�Av�Az�A��A%A�;A?}A�FA7LAoA�A��A��AO�AȴA�A��A�A��A�A=qAt�A
��A
=qA	��A�A5?A��A��A/A�A�AȴA~�A��A��A�/A�DA;dA��A ��A j@��@�M�@��;@�v�@�p�@��H@��u@�hs@�t�@�/@��@�|�@���@�x�@�?}@�u@�b@�@�l�@�v�@��@�/@�Ĝ@�A�@��
@�^5@�p�@�/@��/@�z�@�b@�K�@ޏ\@�`B@ܛ�@ۮ@��H@��@���@��@ץ�@ם�@�33@��@�O�@ԓu@�|�@ҧ�@�-@�hs@��@д9@�;d@�{@͙�@���@̴9@̬@̴9@̴9@̬@̓u@��y@�v�@��@ɉ7@�p�@�G�@���@ȋD@�j@�(�@��@� �@�Z@�Z@� �@��@���@���@�Z@�j@�z�@�j@�A�@��@��
@��H@�33@ƸR@š�@���@�z�@��
@�v�@���@�z�@���@��@�b@��@��@� �@�  @���@�+@�@�33@�33@�;d@�l�@�dZ@�33@�~�@��@��@�@�X@���@�@��@��9@�9X@���@�C�@�@�v�@�$�@�{@��h@�hs@�O�@�O�@��`@���@��m@��@�@�~�@��@��@���@�x�@���@�1'@� �@�1'@��;@�=q@�G�@�V@�V@�V@��@��@�1@��@��@�"�@�@��\@�M�@��@�@��#@���@���@�@���@�?}@�bN@�1@���@�;d@��H@���@��R@�~�@�V@�=q@�@���@�$�@�-@�5?@�{@�{@��@��h@���@�G�@�%@�Ĝ@�z�@�Q�@�I�@�  @���@�+@�@��@���@�@�`B@��D@��;@���@�t�@��@���@�M�@�@���@�Z@���@��P@�
=@���@�M�@��@��@���@��-@�x�@�&�@���@�bN@�1@��@�S�@�
=@���@�ff@�E�@�J@���@�?}@�%@��`@���@���@�j@�A�@�1@���@���@�dZ@�
=@��y@���@�ȴ@��R@���@�E�@���@�@���@���@���@���@�p�@�`B@�`B@�`B@�`B@�`B@�X@�O�@�V@���@�Ĝ@��j@�Q�@��@���@��@���@���@���@��\@��+@�n�@�^5@��@���@���@�X@��@���@�b@�  @��@��w@�C�@��!@�ԕ@�v`@t~(111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	�B	�NB
iyB
�B
�7B
�JB
�uB
��B
��B
��B
�wB
ŢB
��B
��B
�/B
�BB
�B
��B  BJB�B�B�B�B�B$�B49B0!B0!B:^BO�Bm�B�DB��B��B�-B�XB�dB��B��B�B�/B�`B�`B�`B�`B�B��B��B��BB�B&�B)�B,B49B=qB@�BE�BF�BG�BO�BT�BZBYB\)B\)BZBdZBffBcTB`BBZBO�BE�B49B�BVB�B�/B�B��B�XB�oB�BhsB7LB#�BbB
�B
�B
��B
�9B
��B
�B
aHB
B�B
�B
B	�;B	��B	�XB	�B	s�B	M�B	=qB	.B	oB	�B	�B	�B	�B	�B	�B	�B	�B	{B	B�B�yB�5B�#B�
B��BȴB��B�}B�wB�^B�?B�B�B��B��B��B��B��B��B�RB�XB�XB�^B�FB�XB�LB�9B�'B�'B�-B�?B�?B�LB�RB�9B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�3B�RB�dB�wB�}B�}BÖBÖBĜBƨBÖB�qB�LB�LB�LB�qBBĜBƨBBÖB��BBB�}B��BÖBBƨBȴB��B��B��B��B�B�#B�/B�/B�;B�BB�TB�sB�yB�yB�yB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	B	%B	1B		7B	DB	VB	\B	hB	uB	{B	�B	�B	�B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	$�B	%�B	&�B	'�B	)�B	+B	+B	-B	/B	1'B	6FB	9XB	8RB	<jB	D�B	I�B	J�B	K�B	M�B	N�B	O�B	R�B	ZB	YB	`BB	bNB	_;B	\)B	YB	W
B	Q�B	O�B	R�B	XB	[#B	ZB	\)B	^5B	bNB	e`B	e`B	e`B	ffB	iyB	m�B	q�B	u�B	v�B	z�B	� B	~�B	~�B	� B	�B	�DB	�VB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�FB	�RB	�dB	��B	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�)B	�/B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�fB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
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
B
B
B
B
%B
%B
%B
%B
1B
1B
1B
1B
1B
1B
	7B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
\B
PB
5B
*�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B	��B	��B	��B	��B	��B	��B	�B	�NB
iyB
�B
�7B
�JB
�uB
��B
��B
��B
�wB
ŢB
��B
��B
�/B
�BB
�B
��B  BJB�B�B�B�B�B$�B49B0!B0!B:^BO�Bm�B�DB��B��B�-B�XB�dB��B��B�B�/B�`B�`B�`B�`B�B��B��B��BB�B&�B)�B,B49B=qB@�BE�BF�BG�BO�BT�BZBYB\)B\)BZBdZBffBcTB`BBZBO�BE�B49B�BVB�B�/B�B��B�XB�oB�BhsB7LB#�BbB
�B
�B
��B
�9B
��B
�B
aHB
B�B
�B
B	�;B	��B	�XB	�B	s�B	M�B	=qB	.B	oB	�B	�B	�B	�B	�B	�B	�B	�B	{B	B�B�yB�5B�#B�
B��BȴB��B�}B�wB�^B�?B�B�B��B��B��B��B��B��B�RB�XB�XB�^B�FB�XB�LB�9B�'B�'B�-B�?B�?B�LB�RB�9B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�3B�RB�dB�wB�}B�}BÖBÖBĜBƨBÖB�qB�LB�LB�LB�qBBĜBƨBBÖB��BBB�}B��BÖBBƨBȴB��B��B��B��B�B�#B�/B�/B�;B�BB�TB�sB�yB�yB�yB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	B	%B	1B		7B	DB	VB	\B	hB	uB	{B	�B	�B	�B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	$�B	%�B	&�B	'�B	)�B	+B	+B	-B	/B	1'B	6FB	9XB	8RB	<jB	D�B	I�B	J�B	K�B	M�B	N�B	O�B	R�B	ZB	YB	`BB	bNB	_;B	\)B	YB	W
B	Q�B	O�B	R�B	XB	[#B	ZB	\)B	^5B	bNB	e`B	e`B	e`B	ffB	iyB	m�B	q�B	u�B	v�B	z�B	� B	~�B	~�B	� B	�B	�DB	�VB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�FB	�RB	�dB	��B	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�)B	�/B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�fB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
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
B
B
B
B
%B
%B
%B
%B
1B
1B
1B
1B
1B
1B
	7B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
\B
PB
5B
*�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.19 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190550                              AO  ARCAADJP                                                                    20181005190550    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190550  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190550  QCF$                G�O�G�O�G�O�8000            