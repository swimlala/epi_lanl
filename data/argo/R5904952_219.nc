CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:55Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190555  20181005190555  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)��J1   @��*-��@1
��n��c�n��O�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0�C2  C4  C6  C8  C:�C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D fD �fDfD� DfD� DfD� D  Dy�D��Dy�D��Dy�D  D� D  D� D	  D	� D
  D
� D  Dy�D��D� DfD� D  D� D  D� D��Dy�D  D� DfD� D  D� D  D�fDfD� D  D� D  D� D  D�fD  D� D  D� D��Dy�D��Dy�D��D� DfD� D��Dy�D��D y�D ��D!y�D"  D"� D#  D#�fD$fD$�fD%  D%� D&  D&y�D'  D'� D(  D(� D)  D)�fD*  D*y�D*��D+� D,  D,�fD-  D-y�D.  D.� D.��D/� D0  D0� D1  D1� D2  D2� D3  D3y�D3��D4y�D5  D5y�D5��D6� D6��D7� D8  D8�fD9  D9y�D:  D:� D;  D;�fD<  D<� D=  D=� D=��D>y�D?  D?� D@  D@� DAfDA�fDB  DB� DC  DC� DDfDD�fDE  DE� DF  DF� DG  DG� DH  DH�fDIfDI� DJ  DJ� DJ��DK� DLfDL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DQ��DR�fDS  DS� DTfDT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\fD\�fD]  D]� D^  D^� D_fD_� D`  D`y�Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� De��Dfy�Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj�fDk  Dk� DlfDl�fDm  Dm� DnfDn�fDofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dw� Dw�fDy{�D�?�D�	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @>{@�p�@�p�A�RA"�RAB�RAb�RA�\)A�\)A�(�A�\)A�\)A�\)A�\)A�\)B �B�B�B�B �B(G�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�#�B�#�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�=B�W
B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(EC*+�C,+�C.+�C0EC2+�C4+�C6+�C8+�C:EC<EC>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|EC~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C�"�C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C�"�C�"�C�"�C��C��C��C��C��C��C�"�C��C��C��C��C��D GD �GDGD��DGD��DGD��D
�D�{D{D�{D{D�{D
�D��D
�D��D	
�D	��D

�D
��D
�D�{D{D��DGD��D
�D��D
�D��D{D�{D
�D��DGD��D
�D��D
�D�GDGD��D
�D��D
�D��D
�D�GD
�D��D
�D��D{D�{D{D�{D{D��DGD��D{D�{D {D �{D!{D!�{D"
�D"��D#
�D#�GD$GD$�GD%
�D%��D&
�D&�{D'
�D'��D(
�D(��D)
�D)�GD*
�D*�{D+{D+��D,
�D,�GD-
�D-�{D.
�D.��D/{D/��D0
�D0��D1
�D1��D2
�D2��D3
�D3�{D4{D4�{D5
�D5�{D6{D6��D7{D7��D8
�D8�GD9
�D9�{D:
�D:��D;
�D;�GD<
�D<��D=
�D=��D>{D>�{D?
�D?��D@
�D@��DAGDA�GDB
�DB��DC
�DC��DDGDD�GDE
�DE��DF
�DF��DG
�DG��DH
�DH�GDIGDI��DJ
�DJ��DK{DK��DLGDL��DM
�DM��DN{DN��DO
�DO��DP
�DP��DQ
�DQ��DR{DR�GDS
�DS��DTGDT�GDU
�DU��DV
�DV��DW
�DW��DX
�DX��DY
�DY��DZ
�DZ��D[
�D[�GD\GD\�GD]
�D]��D^
�D^��D_GD_��D`
�D`�{Da
�Da��DbGDb��Dc
�Dc��Dd
�Dd��De
�De��Df{Df�{Dg
�Dg��Dh
�Dh��Di
�Di�{Dj
�Dj�GDk
�Dk��DlGDl�GDm
�Dm��DnGDn�GDoGDo��Dp
�Dp��Dq
�Dq��Dr
�Dr��Ds
�Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw��Dw�GDy�fD�ED�]1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�G�A�I�A�I�A�I�A�K�A�G�A�I�A�G�A�I�A�G�A�A�A�=qA�7LA�"�A�VA�1A�1A���AƶFAƋDA���A�+A�G�A�jA�S�A�A�A�+Aǧ�A��TA�oAȋDAȺ^A��AǁA�Q�A�/A�%A���AƝ�A�%A���A¡�A���A��TA���A�5?A�I�A��A�S�A��A�VA��DA��A���A�Q�A�JA�bA�I�A���A�ĜA���A�I�A��hA��;A��9A��uA�A���A��A���A�A�&�A�$�A�7LA�5?A��yA��PA��HA���A��hA�7LA�n�A�VA�1A���A���A��!A���A��PA��A�~�A��A�A��PA�%A��A�z�A��+A�AK�A|��Awx�Ar�jAn��Al�AjA�AhA�Af�HAdE�AbĜAbA�A`��A\=qAY��AVQ�AT�uAR �AP��APA�AO�FAN�jAM\)AJ�+AH�RAFZAD��AD-AAp�A?p�A=A;��A:  A8�A85?A6VA4��A3�hA0�A.9XA,��A,A*  A(�A't�A'oA&E�A$�A#XA"��A!O�A 1'A�
At�A�A�!A��A�DA5?A%A-A�PA�AJA�A�AG�A�AĜAM�AXA�A�A�9Av�A�FA��A&�A��A�A�+AA~�A��AA
�A	�^A5?A��A��AI�A�A&�Az�A��A�wA�PA �`@��m@�dZ@��\@��@���@�ƨ@�M�@��+@��u@���@�v�@�@�@�I�@�ȴ@��@�O�@�?}@��/@�  @�+@��`@�1@�w@�w@�C�@��y@�@�\)@��#@�I�@�5?@���@ݙ�@ݑh@�`B@���@۶F@�l�@ڇ+@�bN@��@�bN@ؓu@�  @׮@�^5@Ցh@Ӿw@�@�&�@�Z@��;@�o@���@Ο�@�$�@��#@�C�@�S�@�1@�Ĝ@ѩ�@�V@ёh@щ7@ЋD@�~�@�A�@˝�@ˮ@�E�@ɉ7@���@�/@ȓu@� �@� �@� �@��m@���@��
@��
@��@�?}@ċD@�1'@��m@�@�$�@�@�V@�r�@�Z@��u@���@��@�%@��@�&�@�/@�7L@�/@��@�/@�V@��`@�Ĝ@��`@���@�1@���@�l�@�"�@��H@���@��T@��#@��^@�V@��P@���@�-@��7@�7L@��D@��@�n�@���@�-@�n�@��H@���@�(�@�;d@�@��R@��+@���@�\)@�@�hs@�(�@�  @�r�@��D@�r�@�I�@�I�@��u@�l�@�33@�33@���@�M�@��@�@�x�@��@�Q�@�z�@��9@�V@���@��@� �@��
@��F@��@���@��@�V@�`B@�hs@��h@�?}@�A�@��@��@��y@��R@�n�@�{@���@�?}@�G�@�O�@��@�r�@�(�@�ƨ@�dZ@�+@�C�@�@���@�n�@�{@�Ĝ@�b@���@�9X@��9@��9@���@�A�@� �@�ƨ@�K�@��y@�n�@���@�v�@�$�@��T@���@��@�/@��/@��u@�b@��;@��F@�l�@�33@��@�ȴ@���@�V@�@���@��h@�Ĝ@�Z@�A�@�A�@�9X@��@�dZ@��R@�-@�$�@���@�?}@�7L@���@���@�(�@��@��;@��w@�t�@�\)@��@�v�@�J@���@�@��7@�7L@��/@���@�r�@�bN@�1'@��F@�|�@�\)@��y@��+@�^5@�=q@���@�x�@��@�V@�V@�%@���@��/@���@�z�@���@��P@�l�@�K�@���@��!@�M�@�J@�@��@�@�p�@�G�@�G�@�/@�%@�r�@��&@|�.@h"h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�G�A�I�A�I�A�I�A�K�A�G�A�I�A�G�A�I�A�G�A�A�A�=qA�7LA�"�A�VA�1A�1A���AƶFAƋDA���A�+A�G�A�jA�S�A�A�A�+Aǧ�A��TA�oAȋDAȺ^A��AǁA�Q�A�/A�%A���AƝ�A�%A���A¡�A���A��TA���A�5?A�I�A��A�S�A��A�VA��DA��A���A�Q�A�JA�bA�I�A���A�ĜA���A�I�A��hA��;A��9A��uA�A���A��A���A�A�&�A�$�A�7LA�5?A��yA��PA��HA���A��hA�7LA�n�A�VA�1A���A���A��!A���A��PA��A�~�A��A�A��PA�%A��A�z�A��+A�AK�A|��Awx�Ar�jAn��Al�AjA�AhA�Af�HAdE�AbĜAbA�A`��A\=qAY��AVQ�AT�uAR �AP��APA�AO�FAN�jAM\)AJ�+AH�RAFZAD��AD-AAp�A?p�A=A;��A:  A8�A85?A6VA4��A3�hA0�A.9XA,��A,A*  A(�A't�A'oA&E�A$�A#XA"��A!O�A 1'A�
At�A�A�!A��A�DA5?A%A-A�PA�AJA�A�AG�A�AĜAM�AXA�A�A�9Av�A�FA��A&�A��A�A�+AA~�A��AA
�A	�^A5?A��A��AI�A�A&�Az�A��A�wA�PA �`@��m@�dZ@��\@��@���@�ƨ@�M�@��+@��u@���@�v�@�@�@�I�@�ȴ@��@�O�@�?}@��/@�  @�+@��`@�1@�w@�w@�C�@��y@�@�\)@��#@�I�@�5?@���@ݙ�@ݑh@�`B@���@۶F@�l�@ڇ+@�bN@��@�bN@ؓu@�  @׮@�^5@Ցh@Ӿw@�@�&�@�Z@��;@�o@���@Ο�@�$�@��#@�C�@�S�@�1@�Ĝ@ѩ�@�V@ёh@щ7@ЋD@�~�@�A�@˝�@ˮ@�E�@ɉ7@���@�/@ȓu@� �@� �@� �@��m@���@��
@��
@��@�?}@ċD@�1'@��m@�@�$�@�@�V@�r�@�Z@��u@���@��@�%@��@�&�@�/@�7L@�/@��@�/@�V@��`@�Ĝ@��`@���@�1@���@�l�@�"�@��H@���@��T@��#@��^@�V@��P@���@�-@��7@�7L@��D@��@�n�@���@�-@�n�@��H@���@�(�@�;d@�@��R@��+@���@�\)@�@�hs@�(�@�  @�r�@��D@�r�@�I�@�I�@��u@�l�@�33@�33@���@�M�@��@�@�x�@��@�Q�@�z�@��9@�V@���@��@� �@��
@��F@��@���@��@�V@�`B@�hs@��h@�?}@�A�@��@��@��y@��R@�n�@�{@���@�?}@�G�@�O�@��@�r�@�(�@�ƨ@�dZ@�+@�C�@�@���@�n�@�{@�Ĝ@�b@���@�9X@��9@��9@���@�A�@� �@�ƨ@�K�@��y@�n�@���@�v�@�$�@��T@���@��@�/@��/@��u@�b@��;@��F@�l�@�33@��@�ȴ@���@�V@�@���@��h@�Ĝ@�Z@�A�@�A�@�9X@��@�dZ@��R@�-@�$�@���@�?}@�7L@���@���@�(�@��@��;@��w@�t�@�\)@��@�v�@�J@���@�@��7@�7L@��/@���@�r�@�bN@�1'@��F@�|�@�\)@��y@��+@�^5@�=q@���@�x�@��@�V@�V@�%@���@��/@���@�z�@���@��P@�l�@�K�@���@��!@�M�@�J@�@��@�@�p�@�G�@�G�@�/@�%@�r�@��&@|�.@h"h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	w�B	w�B	w�B	w�B	w�B	x�B	x�B	y�B	y�B	y�B	}�B	� B	�B	�\B	��B	�LB	ŢB	�B	�mB	�sB
B
�B
"�B
0!B
>wB
<jB
A�B
�B
��B
�-B
�B
��BoB�B?}BM�BO�BM�BN�BI�BM�B�DB��B��B�B�NB��B��BBuB'�B:^BH�BO�BP�BO�BS�B`BBjBl�Bo�Bn�Bl�BjBR�B49B6FB2-B�BuBB�B�BǮB�FB�bB�By�Bk�BYBI�B>wB7LB+B�B�B�B�B{BDB
��B
�;B
��B
ǮB
�RB
�1B
aHB
7LB
�B	��B	�fB	��B	��B	�JB	|�B	q�B	e`B	]/B	P�B	H�B	C�B	7LB	#�B	�B	B��B��B�B�B�B�`B�5B��B��BǮBĜB��B�jB�RB�?B�-B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�!B�'B�-B�-B�?B�XB�RB�dB�wB��B��B��BBƨBǮBȴB��B��B��B��B�
B�B�B��B��B��B��B��B��B��BȴBȴBǮBĜBĜB�^B�'B�B�B�B�B�B�B�!B�'B�9B�LB�RB�RB�RB�LB�RB�LB�?B�9B�RB�qBÖBBB��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�5B�BB�TB�ZB�ZB�TB�`B�`B�`B�B�B�B�B�B�B��B	  B	B	B	B	+B	JB	\B	uB	�B	�B	�B	�B	+B	1'B	A�B	K�B	R�B	ZB	cTB	k�B	m�B	q�B	m�B	ffB	_;B	]/B	^5B	[#B	ZB	[#B	cTB	gmB	hsB	m�B	r�B	t�B	x�B	z�B	z�B	}�B	|�B	|�B	|�B	|�B	~�B	�B	� B	~�B	~�B	�B	�1B	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�3B	�3B	�9B	�FB	�FB	�?B	�9B	�-B	�-B	�-B	�!B	�B	�B	��B	��B	�B	�3B	�dB	�XB	�?B	�3B	�3B	�-B	�-B	�?B	�dB	�RB	�FB	�?B	�RB	�qB	��B	��B	B	ŢB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�/B	�BB	�BB	�BB	�;B	�BB	�BB	�HB	�HB	�BB	�;B	�5B	�;B	�HB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
1B
1B
1B
	7B

=B
DB

=B
JB
JB
JB
PB
PB
VB
VB
bB
bB
hB
hB
\B
\B
VB
\B
bB
hB
oB
oB
oB
oB
uB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
,B
jB
,B
4�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B	w�B	w�B	w�B	w�B	w�B	x�B	x�B	y�B	y�B	y�B	}�B	� B	�B	�\B	��B	�LB	ŢB	�B	�mB	�sB
B
�B
"�B
0!B
>wB
<jB
A�B
�B
��B
�-B
�B
��BoB�B?}BM�BO�BM�BN�BI�BM�B�DB��B��B�B�NB��B��BBuB'�B:^BH�BO�BP�BO�BS�B`BBjBl�Bo�Bn�Bl�BjBR�B49B6FB2-B�BuBB�B�BǮB�FB�bB�By�Bk�BYBI�B>wB7LB+B�B�B�B�B{BDB
��B
�;B
��B
ǮB
�RB
�1B
aHB
7LB
�B	��B	�fB	��B	��B	�JB	|�B	q�B	e`B	]/B	P�B	H�B	C�B	7LB	#�B	�B	B��B��B�B�B�B�`B�5B��B��BǮBĜB��B�jB�RB�?B�-B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�!B�'B�-B�-B�?B�XB�RB�dB�wB��B��B��BBƨBǮBȴB��B��B��B��B�
B�B�B��B��B��B��B��B��B��BȴBȴBǮBĜBĜB�^B�'B�B�B�B�B�B�B�!B�'B�9B�LB�RB�RB�RB�LB�RB�LB�?B�9B�RB�qBÖBBB��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�5B�BB�TB�ZB�ZB�TB�`B�`B�`B�B�B�B�B�B�B��B	  B	B	B	B	+B	JB	\B	uB	�B	�B	�B	�B	+B	1'B	A�B	K�B	R�B	ZB	cTB	k�B	m�B	q�B	m�B	ffB	_;B	]/B	^5B	[#B	ZB	[#B	cTB	gmB	hsB	m�B	r�B	t�B	x�B	z�B	z�B	}�B	|�B	|�B	|�B	|�B	~�B	�B	� B	~�B	~�B	�B	�1B	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�3B	�3B	�9B	�FB	�FB	�?B	�9B	�-B	�-B	�-B	�!B	�B	�B	��B	��B	�B	�3B	�dB	�XB	�?B	�3B	�3B	�-B	�-B	�?B	�dB	�RB	�FB	�?B	�RB	�qB	��B	��B	B	ŢB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�/B	�BB	�BB	�BB	�;B	�BB	�BB	�HB	�HB	�BB	�;B	�5B	�;B	�HB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
1B
1B
1B
	7B

=B
DB

=B
JB
JB
JB
PB
PB
VB
VB
bB
bB
hB
hB
\B
\B
VB
\B
bB
hB
oB
oB
oB
oB
uB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
,B
jB
,B
4�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190555                              AO  ARCAADJP                                                                    20181005190555    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190555  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005190555  QCF$                G�O�G�O�G�O�8000            