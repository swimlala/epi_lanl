CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:00Z creation      
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
resolution        =���   axis      Z        X  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  S(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  \X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  l�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  v   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  @   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181005190600  20181005190600  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)���1   @��*33H@0������c�Z�11   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�  @�  A   A   A>ffA^ffA~ffA�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��3C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C��3C��3C�  C�  C��3C�  C��C��C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  D   D � DfD� D  D�fDfD�fD  D� D  D� D  D�fDfD�fDfD�fD	  D	y�D
  D
�fD  D� D  D� D��D� D  D� D��D� DfDy�D  D�fD  D� D  D�fD  D� D��D� D  D�fDfD� D  Dy�D��D� D  D�fDfD�fD  D� D  D� DfD� D��D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)y�D)��D*� D+fD+� D,  D,� D-fD-� D-��D.� D/  D/� D0fD0� D1  D1y�D1��D2� D3  D3� D3��D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9y�D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?� D@fD@�fDA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DF��DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DU��DV� DW  DWy�DW��DX� DYfDY� DZ  DZ�fD[fD[� D\  D\� D\��D]�fD^  D^� D^��D_y�D`  D`� D`��Da� Db  Db�fDc  Dc� Dd  Dd� Dd��De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dy� D�9�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @>{@�p�@�p�A�RA"�RAA�Aa�A��\A�\)A�\)A�\)A�\)A�\)A�\)A��\B �B�B�B�B �B)zB0�B8�B@�BH�BP�BX�B`�Bh�Bp�ByzB�W
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
B�W
B�W
B�W
B�W
B�W
B�W
B�#�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�C+�C�C+�C
EC+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�CrECt+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��C��C��C��C��C��C�"�C�"�C��C��C��D 
�D ��DGD��D
�D�GDGD�GD
�D��D
�D��D
�D�GDGD�GDGD�GD	
�D	�{D

�D
�GD
�D��D
�D��D{D��D
�D��D{D��DGD�{D
�D�GD
�D��D
�D�GD
�D��D{D��D
�D�GDGD��D
�D�{D{D��D
�D�GDGD�GD
�D��D
�D��DGD��D{D��D 
�D ��D!{D!��D"
�D"��D#
�D#��D$
�D$��D%
�D%��D&
�D&�GD'
�D'��D(
�D(��D)
�D)�{D*{D*��D+GD+��D,
�D,��D-GD-��D.{D.��D/
�D/��D0GD0��D1
�D1�{D2{D2��D3
�D3��D4{D4��D5
�D5�GD6
�D6��D7
�D7��D8
�D8��D9
�D9�{D:
�D:�{D;
�D;��D<
�D<��D=
�D=��D>
�D>�{D?
�D?��D@GD@�GDA
�DA��DB
�DB��DC
�DC��DD
�DD��DEGDE��DF
�DF��DG{DG��DH
�DH��DIGDI��DJ
�DJ��DK
�DK��DL
�DL��DM
�DM��DN
�DN��DO
�DO��DP
�DP��DQ
�DQ��DR
�DR��DS
�DS��DTGDT��DU
�DU��DV{DV��DW
�DW�{DX{DX��DYGDY��DZ
�DZ�GD[GD[��D\
�D\��D]{D]�GD^
�D^��D_{D_�{D`
�D`��Da{Da��Db
�Db�GDc
�Dc��Dd
�Dd��De{De��Df
�Df��DgGDg��Dh
�Dh��Di
�Di��Dy��D�?D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��mA��A��A��A��A��A��#AʑhA�JA�p�Aȕ�AǋDA���A���AƶFA�VAŝ�A�M�A�bA��yA��mA��mA��yA��yA��mA��yA���A�  A�%A��A�&�A� �A�"�A�$�A�l�Aş�AōPA�v�A�hsA�ZA�K�A�33A�{A�A�%A�|�A�
=A�A���A�n�A�$�A��yA��A�x�A�VA�{A��^A�v�A�(�A���A���A�%A�Q�A��-A�ȴA��RA�+A�r�A�VA��mA�ƨA�^5A�&�A�ffA��A���A�bNA�-A�{A���A��A�I�A���A��A�A��A�&�A�(�A�1A���A���A���A��9A�C�A���A���A��-A�l�A���A�O�A��HA}��Av��Aq��AqoAp��AnZAk�AgoAb�RAa��A_O�A\AY�AWC�AV�AVQ�AVE�AVVAU|�ATbNAR(�AP �ANA�AL��AH�+AFffAF �ADI�A=�-A9��A9C�A9"�A8�A733A3�A1x�A/�wA.v�A,�jA,A�A*��A)��A(�A'|�A&�!A&v�A%��A$��A#�7A#33A"ĜA!�^A �A��Ax�A�;A��AC�A/A$�A�A�DA��A�Al�A~�AĜA�A��A`BAȴA�A1'A��A\)AVA�!A��A
VA	�A	S�A��A`BA��A�A�
AM�AO�A {@���@�;d@�@��7@�`B@��`@�S�@�+@��
@��/@���@���@���@�\@���@@�ff@�K�@�@�"�@�ȴ@�E�@�h@��@�P@��@���@���@�@��m@��H@�-@�F@�dZ@�^5@���@�/@�I�@�+@��@�~�@�@��@��@ە�@�\)@�33@�ȴ@���@�Ĝ@�l�@�C�@�t�@֧�@�p�@ԓu@ԋD@ӍP@�r�@�I�@�@��@��@�%@Ϯ@�{@���@̋D@�A�@��
@��T@�V@���@ȃ@� �@�1@��@ǶF@�dZ@��@��y@�@�@Ɨ�@��@�@��#@Ų-@Ĵ9@ă@��@�C�@�@� �@��P@��@�^5@�@���@��@��@�C�@��y@��@��T@��h@�X@�%@�A�@�|�@�dZ@�33@�Q�@��@��T@�7L@�Ĝ@�1'@��F@�ƨ@��@���@�ff@�O�@��^@�@�5?@�@��@�V@���@�p�@��@��j@�A�@��@�@�Q�@�ƨ@���@��@�K�@�ȴ@��+@�M�@���@��j@��;@��
@��;@��F@���@�\)@�;d@�+@�@��y@��y@�@��H@���@��^@�O�@���@�A�@�1@��@���@�t�@�|�@��@��P@�S�@���@��@�?}@���@��j@��D@�bN@�  @��P@�K�@��@��R@���@��\@��\@�M�@�-@��@�J@��@���@��^@�Ĝ@���@���@��9@�z�@� �@�dZ@�S�@�;d@�
=@��y@��R@�-@���@���@��@�?}@�/@�V@���@��/@���@�z�@�Q�@�1@��;@���@���@��R@��+@��+@�~�@�M�@�{@�@�J@�{@�J@���@�`B@�V@�V@�%@��@�b@��@�C�@��H@���@���@�ff@�{@��#@��@��@��@�9X@��;@��P@�|�@��@�dZ@�C�@�"�@��y@��H@���@���@��+@�=q@�{@��#@���@�hs@�/@��9@�b@���@�@���@���@���@��{@���@ox11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��mA��A��A��A��A��A��#AʑhA�JA�p�Aȕ�AǋDA���A���AƶFA�VAŝ�A�M�A�bA��yA��mA��mA��yA��yA��mA��yA���A�  A�%A��A�&�A� �A�"�A�$�A�l�Aş�AōPA�v�A�hsA�ZA�K�A�33A�{A�A�%A�|�A�
=A�A���A�n�A�$�A��yA��A�x�A�VA�{A��^A�v�A�(�A���A���A�%A�Q�A��-A�ȴA��RA�+A�r�A�VA��mA�ƨA�^5A�&�A�ffA��A���A�bNA�-A�{A���A��A�I�A���A��A�A��A�&�A�(�A�1A���A���A���A��9A�C�A���A���A��-A�l�A���A�O�A��HA}��Av��Aq��AqoAp��AnZAk�AgoAb�RAa��A_O�A\AY�AWC�AV�AVQ�AVE�AVVAU|�ATbNAR(�AP �ANA�AL��AH�+AFffAF �ADI�A=�-A9��A9C�A9"�A8�A733A3�A1x�A/�wA.v�A,�jA,A�A*��A)��A(�A'|�A&�!A&v�A%��A$��A#�7A#33A"ĜA!�^A �A��Ax�A�;A��AC�A/A$�A�A�DA��A�Al�A~�AĜA�A��A`BAȴA�A1'A��A\)AVA�!A��A
VA	�A	S�A��A`BA��A�A�
AM�AO�A {@���@�;d@�@��7@�`B@��`@�S�@�+@��
@��/@���@���@���@�\@���@@�ff@�K�@�@�"�@�ȴ@�E�@�h@��@�P@��@���@���@�@��m@��H@�-@�F@�dZ@�^5@���@�/@�I�@�+@��@�~�@�@��@��@ە�@�\)@�33@�ȴ@���@�Ĝ@�l�@�C�@�t�@֧�@�p�@ԓu@ԋD@ӍP@�r�@�I�@�@��@��@�%@Ϯ@�{@���@̋D@�A�@��
@��T@�V@���@ȃ@� �@�1@��@ǶF@�dZ@��@��y@�@�@Ɨ�@��@�@��#@Ų-@Ĵ9@ă@��@�C�@�@� �@��P@��@�^5@�@���@��@��@�C�@��y@��@��T@��h@�X@�%@�A�@�|�@�dZ@�33@�Q�@��@��T@�7L@�Ĝ@�1'@��F@�ƨ@��@���@�ff@�O�@��^@�@�5?@�@��@�V@���@�p�@��@��j@�A�@��@�@�Q�@�ƨ@���@��@�K�@�ȴ@��+@�M�@���@��j@��;@��
@��;@��F@���@�\)@�;d@�+@�@��y@��y@�@��H@���@��^@�O�@���@�A�@�1@��@���@�t�@�|�@��@��P@�S�@���@��@�?}@���@��j@��D@�bN@�  @��P@�K�@��@��R@���@��\@��\@�M�@�-@��@�J@��@���@��^@�Ĝ@���@���@��9@�z�@� �@�dZ@�S�@�;d@�
=@��y@��R@�-@���@���@��@�?}@�/@�V@���@��/@���@�z�@�Q�@�1@��;@���@���@��R@��+@��+@�~�@�M�@�{@�@�J@�{@�J@���@�`B@�V@�V@�%@��@�b@��@�C�@��H@���@���@�ff@�{@��#@��@��@��@�9X@��;@��P@�|�@��@�dZ@�C�@�"�@��y@��H@���@���@��+@�=q@�{@��#@���@�hs@�/@��9@�b@���@�@���@���@���@��{@���@ox11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B(�B?}BYB� B��B�3B�FB��BŢB��BƨBÖBȴB��B��B��B��B��B��B�#B�BB�fB	%B	7LB	YB	ZB	^5B	� B	��B	�!B	��B	�B
B
"�B
r�B
��B
��B
��B�B;dB<jB>wBM�BR�BYBe`Bn�Bw�B�PB��B�B�^B�wBB��B�B�5B�fB�B��BB��B��B��B��B�B�B�B�B�B�B�B�B�B�B�sB�NB�;B��B�'B��B��B�BhsBQ�B!�B
�BB
�{B
(�B	�B	ȴB	��B	�oB	{�B	\)B	8RB	�B	�B	{B	+B��B�`B�B��BȴBBÖBĜB�)B�;B�HB�HB��B	hB	$�B	"�B	�B	�B	VB	DB		7B��B��B�LB�9B�-B�!B��B��B��B��B��B�B�B�B�'B�LB�dB�^B�XB�LB�FB�RB�RB�RB�dB�qB�}B��BBĜBÖB��B�wB�dB�9B�-B�'B�!B�!B�B�B��B�!B�dB��B��B��B��B��B�wB�XB�?B�3B�3B�FB�LB�3B�B��B��B��B��B��B��B��B��B�B�XB�qBǮB��B�TB�mB�B�B�B�B��B��B	B		7B		7B	1B	+B	%B	1B		7B	DB	DB	DB	
=B	
=B	DB	PB	hB	�B	�B	�B	"�B	&�B	'�B	-B	/B	/B	1'B	2-B	49B	6FB	8RB	9XB	:^B	6FB	1'B	33B	6FB	49B	49B	=qB	E�B	G�B	P�B	VB	S�B	S�B	S�B	[#B	^5B	^5B	^5B	aHB	e`B	hsB	aHB	dZB	jB	k�B	jB	jB	jB	k�B	l�B	o�B	p�B	r�B	t�B	u�B	w�B	y�B	z�B	|�B	~�B	�B	�B	�B	�B	}�B	}�B	}�B	{�B	{�B	{�B	�B	�B	~�B	~�B	}�B	~�B	�B	� B	~�B	~�B	� B	� B	�B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�?B	�XB	�^B	�LB	�3B	�-B	�9B	�?B	�?B	�LB	�RB	�RB	�^B	�^B	�dB	�qB	��B	ĜB	ĜB	ŢB	ǮB	ɺB	ɺB	ɺB	��B	��B	�B	�B	�B	�B	�B	��B	�
B	�
B	�
B	�B	�/B	�;B	�BB	�HB	�HB	�BB	�;B	�;B	�BB	�HB	�HB	�HB	�BB	�NB	�TB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
B
B
%B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
DB
PB
VB
VB
VB
VB
VB
PB
PB
VB
\B
B
/OB
>(22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�B�B�B�B�B�B�B(�B?}BYB� B��B�3B�FB��BŢB��BƨBÖBȴB��B��B��B��B��B��B�#B�BB�fB	%B	7LB	YB	ZB	^5B	� B	��B	�!B	��B	�B
B
"�B
r�B
��B
��B
��B�B;dB<jB>wBM�BR�BYBe`Bn�Bw�B�PB��B�B�^B�wBB��B�B�5B�fB�B��BB��B��B��B��B�B�B�B�B�B�B�B�B�B�B�sB�NB�;B��B�'B��B��B�BhsBQ�B!�B
�BB
�{B
(�B	�B	ȴB	��B	�oB	{�B	\)B	8RB	�B	�B	{B	+B��B�`B�B��BȴBBÖBĜB�)B�;B�HB�HB��B	hB	$�B	"�B	�B	�B	VB	DB		7B��B��B�LB�9B�-B�!B��B��B��B��B��B�B�B�B�'B�LB�dB�^B�XB�LB�FB�RB�RB�RB�dB�qB�}B��BBĜBÖB��B�wB�dB�9B�-B�'B�!B�!B�B�B��B�!B�dB��B��B��B��B��B�wB�XB�?B�3B�3B�FB�LB�3B�B��B��B��B��B��B��B��B��B�B�XB�qBǮB��B�TB�mB�B�B�B�B��B��B	B		7B		7B	1B	+B	%B	1B		7B	DB	DB	DB	
=B	
=B	DB	PB	hB	�B	�B	�B	"�B	&�B	'�B	-B	/B	/B	1'B	2-B	49B	6FB	8RB	9XB	:^B	6FB	1'B	33B	6FB	49B	49B	=qB	E�B	G�B	P�B	VB	S�B	S�B	S�B	[#B	^5B	^5B	^5B	aHB	e`B	hsB	aHB	dZB	jB	k�B	jB	jB	jB	k�B	l�B	o�B	p�B	r�B	t�B	u�B	w�B	y�B	z�B	|�B	~�B	�B	�B	�B	�B	}�B	}�B	}�B	{�B	{�B	{�B	�B	�B	~�B	~�B	}�B	~�B	�B	� B	~�B	~�B	� B	� B	�B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�?B	�XB	�^B	�LB	�3B	�-B	�9B	�?B	�?B	�LB	�RB	�RB	�^B	�^B	�dB	�qB	��B	ĜB	ĜB	ŢB	ǮB	ɺB	ɺB	ɺB	��B	��B	�B	�B	�B	�B	�B	��B	�
B	�
B	�
B	�B	�/B	�;B	�BB	�HB	�HB	�BB	�;B	�;B	�BB	�HB	�HB	�HB	�BB	�NB	�TB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
B
B
%B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
DB
PB
VB
VB
VB
VB
VB
PB
PB
VB
\B
B
/OB
>(22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190600                              AO  ARCAADJP                                                                    20181005190600    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190600  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190600  QCF$                G�O�G�O�G�O�8000            