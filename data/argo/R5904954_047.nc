CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:59Z creation      
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
resolution        =���   axis      Z        8  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  Bx   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  R�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  [�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  d�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  l    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  s8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  u   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  ~   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20181005191659  20181005191659  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               /A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׻$�Bq1   @׻%����@5Z��vȴ�c�+J1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      /A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8�C:  C;�fC>  C@  CB  CD  CF�CH�CJ  CL  CN  CP  CR  CT  CV  CW�fCY�fC[�fC^  C`  Cb  Cd  Cf  Ch�Cj  Ck�fCm�fCp  Cr  Ct  Cu�fCx  Cy�fC|  C~  C��C�  C��3C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��3C��3C��3C�  C��3C��3C��3C��C��3C�  C��C��C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C��C�  C��3C�  C��C��C�  C�  C��C��C��3C��3C��3C��3C�  C�  C�  C��3C�  C��C�  C�  C�  C��C�  C��C��3C�  C��C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C��D fD � D  D� D��D� D  D� D��D� D  D� D��D� D  D� D  D�fD	  D	y�D
  D
y�D
��D� D  D� D  D� DfD� D  D� D  D�fD  Dy�D��D� DfDy�D  D�fD  D� D  Dy�D  D� D  D� D��D� DfD� D  D�fD  Dy�D  D� D  D� D  D� D��D � D!fD!�fD"�D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'�fD(  D(� D(��D)� D*fD*�fD*��D+s3D,  D,�fD-fD-�fD.  D.� D/  D/� D/��D0� D1  D1y�D2  D2�fD3  D3y�D4  D4� D5  D5� D6  D6y�D6��D7� D8fD8� D9  D9� D:fD:�fD;fD;� D<fD<� D=  D=� D=��D>y�D?  D?�fD@  D@y�DA  DA� DA��DBy�DCfDC�fDD  DD�fDE  DE� DF  DF� DGfDG� DG��DH�fDI  DI� DJ  DJy�DJ��DKy�DL  DL�fDMfDM� DM��DN� DOfDO� DP  DPy�DQ  DQ� DQ��DRy�DR��DSy�DT  DT�fDU  DU� DU��DVy�DV��DW� DX  DXy�DY  DY� DZ  DZ� DZ��D[y�D\  D\�fD]fD]�fD^fD^�fD_fD_�fD`  D`y�Da  Da� Da��Db� Dc  Dcy�Dd  Dd� De  De� Df  Dy��D�HRD�o
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@��HAp�A!p�AAp�Aap�A��RA��RA��RA��RA��RAиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�aGB���B���B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�aGC 
C
C
C
C
C

C
C
C
C
C
C
C
C
C
C
C 
C"
C$
C&
C(0�C*
C,
C.
C0
C2
C4
C6
C80�C:
C;�pC>
C@
CB
CD
CF0�CH0�CJ
CL
CN
CP
CR
CT
CV
CW�pCY�pC[�pC^
C`
Cb
Cd
Cf
Ch0�Cj
Ck�pCm�pCp
Cr
Ct
Cu�pCx
Cy�pC|
C~
C�RC��C���C��C��C���C��C�RC��C��C�RC��C��C��C��C��C���C��C���C���C���C��C���C���C���C�RC���C��C�RC�RC���C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC���C��C��C��C��C��C��C��C���C���C��C��C��C��C���C��C��C��C��C�RC�RC�RC��C���C��C�RC�RC��C��C�RC�RC���C���C���C���C��C��C��C���C��C�RC��C��C��C�RC��C�RC���C��C�RC��C��C��C��C�RC��C���C���C��C��C��C��C��C��C�RC�RC�RC�RC�RC��C��C��C���C���C��C��C�RC��C��C�RD )D ��D�D��D�]D��D�D��D�]D��D�D��D�]D��D�D��D�D�)D	�D	]D
�D
]D
�]D��D�D��D�D��D)D��D�D��D�D�)D�D]D�]D��D)D]D�D�)D�D��D�D]D�D��D�D��D�]D��D)D��D�D�)D�D]D�D��D�D��D�D��D�]D ��D!)D!�)D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D')D'�)D(�D(��D(�]D)��D*)D*�)D*�]D+x�D,�D,�)D-)D-�)D.�D.��D/�D/��D/�]D0��D1�D1]D2�D2�)D3�D3]D4�D4��D5�D5��D6�D6]D6�]D7��D8)D8��D9�D9��D:)D:�)D;)D;��D<)D<��D=�D=��D=�]D>]D?�D?�)D@�D@]DA�DA��DA�]DB]DC)DC�)DD�DD�)DE�DE��DF�DF��DG)DG��DG�]DH�)DI�DI��DJ�DJ]DJ�]DK]DL�DL�)DM)DM��DM�]DN��DO)DO��DP�DP]DQ�DQ��DQ�]DR]DR�]DS]DT�DT�)DU�DU��DU�]DV]DV�]DW��DX�DX]DY�DY��DZ�DZ��DZ�]D[]D\�D\�)D])D]�)D^)D^�)D_)D_�)D`�D`]Da�Da��Da�]Db��Dc�Dc]Dd�Dd��De�De��Df�Dy�qD�K3D�q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�+A�1'A�5?A�;dA�9XA�=qA�A�A�C�A�E�A�E�A�G�A�G�A�C�A�E�A�G�A�I�A�A�A�33A�=qA�9XA�%Aם�A�A�VA�5?A��HAա�A�A�A�\)A�;dA��TAϋDAΛ�A̓A̸RA˼jA�t�A���A��#A�K�AȁA��A�n�AƾwA�t�AżjA�;dA�
=A���A�I�A���A� �A�Q�A�"�A�1'A��A���A��A��hA��A�-A��A�K�A�9XA��A�E�A��TA��A��uA��jA���A��!A�
=A�p�A�x�A���A�1A�33A���A��A�/A�&�A���A�A�A���A�1'A�A��A�bNA�  A���A�I�A���A�E�A���A��A���A�jA�A�1A�1A���A���A�ĜA��/A�PA~�A}�PA{VAy;dAwC�Au7LApI�AlAjJAh��Af�Aa��A_"�A]�#A]oA[33AZQ�AY?}AW�AV�AUXATQ�AR�AOG�ALI�AJ��AHz�AFffAF=qAE�7ADACC�AB�HAA�FA>��A<A�A:I�A9O�A7�mA6��A6ffA5�mA4A1��A0jA/��A/�A,�\A+��A+\)A+G�A*�A)ƨA(�`A(bNA&�A#�A!?}Ax�A�HA~�A1'A��A?}A"�A��A(�A��A7LA�9A�PA"�A��A�FAO�A�`AA�A�A�^A�A�yAv�A�A��A�AA�A��A�+A��A�;A|�AO�Al�A
��A	\)A$�A�wAv�A�^A�A
=Az�AI�At�A$�A\)A �!A -@�;d@���@�J@��@�%@��@�5?@�E�@���@���@�ff@���@�@��@��@�;d@�@��@땁@�K�@�
=@�=q@�@�dZ@��@�@�@��@�@ާ�@��/@ڗ�@�`B@�Ĝ@؋D@�A�@���@׶F@ם�@�t�@�\)@�v�@��@ա�@���@�Q�@�(�@�b@��@�K�@��@�X@�%@мj@�A�@��@�p�@�X@�/@�(�@�"�@���@�V@ɩ�@�r�@�;d@�V@��@���@å�@�E�@�Z@���@�\)@�v�@��@�r�@��;@�dZ@�\)@�\)@�C�@�"�@�"�@��!@�$�@��-@��-@�/@���@��9@�1@��F@���@��@���@�E�@���@�?}@��D@�ƨ@��@�v�@�5?@�@���@���@��@�x�@�X@�G�@�V@���@���@�S�@�~�@���@�O�@�/@�&�@��@�V@���@���@���@��@��@��`@��`@��/@�Ĝ@�b@���@�l�@�33@��y@�~�@��T@�hs@���@�Q�@�(�@��;@���@�\)@�+@�o@��y@��!@�J@�?}@���@��@�r�@�bN@�Q�@�Q�@�I�@�A�@�1'@�b@�b@��m@��@��H@��R@��+@�v�@�E�@��#@���@��@�O�@��@���@��@�z�@�(�@���@���@��P@�l�@�\)@�\)@�K�@��@��y@�=q@�J@���@�x�@�?}@���@���@�Q�@�Q�@�9X@���@�C�@��y@�^5@�=q@�=q@�{@���@�x�@�G�@���@��j@�I�@��F@�dZ@�;d@�
=@���@��y@��@�ȴ@��!@��!@���@���@��\@��+@�v�@�M�@�$�@���@��@�G�@��@�V@��@���@��u@�A�@���@��@���@�^5@�=q@��@��#@�x�@�`B@�W�@~�<@s��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�+A�1'A�5?A�;dA�9XA�=qA�A�A�C�A�E�A�E�A�G�A�G�A�C�A�E�A�G�A�I�A�A�A�33A�=qA�9XA�%Aם�A�A�VA�5?A��HAա�A�A�A�\)A�;dA��TAϋDAΛ�A̓A̸RA˼jA�t�A���A��#A�K�AȁA��A�n�AƾwA�t�AżjA�;dA�
=A���A�I�A���A� �A�Q�A�"�A�1'A��A���A��A��hA��A�-A��A�K�A�9XA��A�E�A��TA��A��uA��jA���A��!A�
=A�p�A�x�A���A�1A�33A���A��A�/A�&�A���A�A�A���A�1'A�A��A�bNA�  A���A�I�A���A�E�A���A��A���A�jA�A�1A�1A���A���A�ĜA��/A�PA~�A}�PA{VAy;dAwC�Au7LApI�AlAjJAh��Af�Aa��A_"�A]�#A]oA[33AZQ�AY?}AW�AV�AUXATQ�AR�AOG�ALI�AJ��AHz�AFffAF=qAE�7ADACC�AB�HAA�FA>��A<A�A:I�A9O�A7�mA6��A6ffA5�mA4A1��A0jA/��A/�A,�\A+��A+\)A+G�A*�A)ƨA(�`A(bNA&�A#�A!?}Ax�A�HA~�A1'A��A?}A"�A��A(�A��A7LA�9A�PA"�A��A�FAO�A�`AA�A�A�^A�A�yAv�A�A��A�AA�A��A�+A��A�;A|�AO�Al�A
��A	\)A$�A�wAv�A�^A�A
=Az�AI�At�A$�A\)A �!A -@�;d@���@�J@��@�%@��@�5?@�E�@���@���@�ff@���@�@��@��@�;d@�@��@땁@�K�@�
=@�=q@�@�dZ@��@�@�@��@�@ާ�@��/@ڗ�@�`B@�Ĝ@؋D@�A�@���@׶F@ם�@�t�@�\)@�v�@��@ա�@���@�Q�@�(�@�b@��@�K�@��@�X@�%@мj@�A�@��@�p�@�X@�/@�(�@�"�@���@�V@ɩ�@�r�@�;d@�V@��@���@å�@�E�@�Z@���@�\)@�v�@��@�r�@��;@�dZ@�\)@�\)@�C�@�"�@�"�@��!@�$�@��-@��-@�/@���@��9@�1@��F@���@��@���@�E�@���@�?}@��D@�ƨ@��@�v�@�5?@�@���@���@��@�x�@�X@�G�@�V@���@���@�S�@�~�@���@�O�@�/@�&�@��@�V@���@���@���@��@��@��`@��`@��/@�Ĝ@�b@���@�l�@�33@��y@�~�@��T@�hs@���@�Q�@�(�@��;@���@�\)@�+@�o@��y@��!@�J@�?}@���@��@�r�@�bN@�Q�@�Q�@�I�@�A�@�1'@�b@�b@��m@��@��H@��R@��+@�v�@�E�@��#@���@��@�O�@��@���@��@�z�@�(�@���@���@��P@�l�@�\)@�\)@�K�@��@��y@�=q@�J@���@�x�@�?}@���@���@�Q�@�Q�@�9X@���@�C�@��y@�^5@�=q@�=q@�{@���@�x�@�G�@���@��j@�I�@��F@�dZ@�;d@�
=@���@��y@��@�ȴ@��!@��!@���@���@��\@��+@�v�@�M�@�$�@���@��@�G�@��@�V@��@���@��u@�A�@���@��@���@�^5@�=q@��@��#@�x�@�`B@�W�@~�<@s��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBBBBBBBBBBBBBBBBBBBBB%B�B#�B'�B49B:^BD�B`BBw�B��B��B��B��B�B�B�!B�'B�LB��B��B�
B�HB�yB�B��BJB�B&�B+B'�B.B=qBJ�BM�BZBjBq�BjBaHBdZBr�Bp�B�B�JB�uB��B��B��B�DB�PB�DB�1B�bB��B��B��B�DBu�Bm�BbNBW
BT�BO�BB�B7LB�B��B�B��B�B��B��B�VB{�B^5BM�B�B
�B
�;B
��B
�!B
��B
v�B
bNB
K�B
D�B
9XB
$�B
�B
%B	��B	�
B	�qB	�B	��B	��B	}�B	n�B	ffB	aHB	W
B	P�B	J�B	B�B	:^B	49B	/B	!�B	{B	+B	  B��B�B�B�B�`B�NB�;B�B��BȴBB�}B�dB�XB�RB�?B�'B�B��B��B��B��B��B��B��B��B��B��B��B�hB�\B��B��B��B��B��B��B��B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B�B�FB�XB�jB�qB��BBBƨBƨBƨBŢBĜBĜBÖB��B�XB�9B�RB�qBɺBǮB�}B�jB�?B�!B�B�!B�'B�-B�?B�?B�FB�FB�FB�XB�qB�}B��BBBBBBƨBɺB��B��B��B��B��B�#B�HB�ZB�sB�yB�sB�mB�mB�sB�yB�B�B�B��B	B	B	+B	JB	hB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	)�B	/B	/B	0!B	2-B	33B	49B	8RB	:^B	=qB	@�B	B�B	G�B	I�B	N�B	T�B	W
B	ZB	[#B	]/B	^5B	_;B	`BB	aHB	bNB	ffB	k�B	o�B	s�B	w�B	z�B	z�B	{�B	{�B	{�B	{�B	|�B	|�B	|�B	|�B	|�B	|�B	}�B	}�B	�B	�B	�B	�B	�%B	�+B	�7B	�=B	�PB	�VB	�\B	�bB	�hB	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�'B	�'B	�3B	�?B	�?B	�LB	�RB	�^B	�^B	�jB	�qB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
 B
�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BBBBBBBBBBBBBBBBBBBBBB%B�B#�B'�B49B:^BD�B`BBw�B��B��B��B��B�B�B�!B�'B�LB��B��B�
B�HB�yB�B��BJB�B&�B+B'�B.B=qBJ�BM�BZBjBq�BjBaHBdZBr�Bp�B�B�JB�uB��B��B��B�DB�PB�DB�1B�bB��B��B��B�DBu�Bm�BbNBW
BT�BO�BB�B7LB�B��B�B��B�B��B��B�VB{�B^5BM�B�B
�B
�;B
��B
�!B
��B
v�B
bNB
K�B
D�B
9XB
$�B
�B
%B	��B	�
B	�qB	�B	��B	��B	}�B	n�B	ffB	aHB	W
B	P�B	J�B	B�B	:^B	49B	/B	!�B	{B	+B	  B��B�B�B�B�`B�NB�;B�B��BȴBB�}B�dB�XB�RB�?B�'B�B��B��B��B��B��B��B��B��B��B��B��B�hB�\B��B��B��B��B��B��B��B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B�B�FB�XB�jB�qB��BBBƨBƨBƨBŢBĜBĜBÖB��B�XB�9B�RB�qBɺBǮB�}B�jB�?B�!B�B�!B�'B�-B�?B�?B�FB�FB�FB�XB�qB�}B��BBBBBBƨBɺB��B��B��B��B��B�#B�HB�ZB�sB�yB�sB�mB�mB�sB�yB�B�B�B��B	B	B	+B	JB	hB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	)�B	/B	/B	0!B	2-B	33B	49B	8RB	:^B	=qB	@�B	B�B	G�B	I�B	N�B	T�B	W
B	ZB	[#B	]/B	^5B	_;B	`BB	aHB	bNB	ffB	k�B	o�B	s�B	w�B	z�B	z�B	{�B	{�B	{�B	{�B	|�B	|�B	|�B	|�B	|�B	|�B	}�B	}�B	�B	�B	�B	�B	�%B	�+B	�7B	�=B	�PB	�VB	�\B	�bB	�hB	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�'B	�'B	�3B	�?B	�?B	�LB	�RB	�^B	�^B	�jB	�qB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
 B
�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191659                              AO  ARCAADJP                                                                    20181005191659    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191659  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191659  QCF$                G�O�G�O�G�O�8000            