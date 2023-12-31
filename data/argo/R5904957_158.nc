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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140834  20181024140834  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$��V1   @��%y\��@5yXbM��c�-V1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv�Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D�fD  D� D  D� DfD� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)fD)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;fD;� D<  D<� D=  D=y�D=��D>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DEfDE�fDF  DF� DG  DGy�DG��DHy�DI  DI� DI��DJy�DJ��DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDrfDr� Ds  Ds� Dt  Dt�fDufDu� Dv  Dv� Dw  Dw� Dx  DxL�Dy�qD�6�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@��HAp�A!p�AAp�Aap�A��RA��RA��RA��RA��AиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B���B�.B�.B�.B�.B�.B�.B�.B���B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�.B�.B�.B�.C 
C
C
C
C
C

C
C
C0�C0�C
C
C
C
C
C
C 
C"
C$
C&
C(
C*
C,
C.
C0
C2
C4
C6
C8
C:
C<
C>
C@
CB
CD
CF
CH
CJ
CL
CN
CP
CR
CT
CV
CX
CY�pC\
C^
C`
Cb
Cd
Cf
Ch
Cj
Cl
Cn
Cp
Cr
Ct0�Cv0�Cx
Cz
C{�pC~
C��C��C��C��C��C��C��C��C��C��C��C���C���C���C��C��C��C��C���C���C��C��C��C�RC��C��C���C��C��C��C��C��C��C��C��C��C���C���C��C��C��C�RC�RC�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C�RC��C��C��C�RC�RC�RC��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC�RD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D]D�D�)D�D��D�D��D)D��D�D��D�D��D�D��D�]D ��D!�D!��D"�D"��D#�D#��D$�D$�)D%�D%��D&�D&��D'�D'��D(�D(��D))D)�)D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0)D0�)D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9�)D:�D:��D;)D;��D<�D<��D=�D=]D=�]D>��D?�D?��D@�D@��DA�DA�)DB�DB��DC�DC��DD�DD��DE)DE�)DF�DF��DG�DG]DG�]DH]DI�DI��DI�]DJ]DJ�]DK��DL�DL��DM)DM��DN�DN��DO�DO��DP�DP]DQ�DQ��DR�DR��DS�DS]DT�DT��DU�DU��DV�DV��DW�DW��DX�DX]DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db]Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg]Dh�Dh�)Di�Di��Dj�Dj��Dk�Dk�)Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq�)Dr)Dr��Ds�Ds��Dt�Dt�)Du)Du��Dv�Dv��Dw�Dw��Dx�DxR�Dy�4D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��mA��mA��A��yA��A��A��A��A��A��A��A�JA�JA�JA�A��A��A��;A�ȴAԮAԗ�Aԇ+A�t�A�ffA�ƨA��Aχ+A���AċDA¸RA��^A�I�A�
=A�1A�x�A��A��9A��wA��A�oA�+A� �A��!A���A��A��A�VA���A�oA�~�A�{A�t�A�dZA���A��A�33A�A��A�S�A��A��A���A�I�A���A��A�(�A�^5A���A�I�A��A��+A�  A���A���A���A�|�A�-A�E�A�E�A�5?A�$�A��A���A�^5A���A��A���A��A��A�1A��;A�r�A���A{/Ay�;Axv�Aw��AwdZAw�Av�AvffAt9XAn�jAn{Am��AmC�Al9XAh�HAe��Abn�A^JA[oAZ�+AX1'ASS�AQ�-AP��AN��AMx�AL�HAK��AKhsAKAI�TAH�AF1ADffAC\)AA��A@�A?��A>��A<�yA:�jA9|�A8VA6��A5A3XA3?}A3/A2ȴA2ffA1�PA/�hA-x�A-VA,ȴA,�\A,�A+K�A*r�A)�7A)l�A)S�A)&�A(�/A'�^A%�wA"ȴA!�A   AĜA"�A�hA5?A\)AS�A��A��A�A`BA(�AJAp�A�7A�A=qAXA\)A/A�AA
9XA�/A�jA�uAffAA�A(�A{A{A1A�mA��A�hAC�A�A��A�A��A�AbNAdZA�AI�At�A �@���@�M�@�`B@���@��-@�&�@�z�@��;@���@�b@�V@�j@���@���@�V@�"�@�h@�1'@�7L@�w@��y@�!@�$�@��`@߅@�=q@��T@�`B@ܼj@���@�&�@ؓu@�b@��;@׍P@֧�@Չ7@Ԭ@�b@ӍP@�@�n�@Гu@��@�~�@�V@�$�@��@Ͳ-@�O�@̴9@��@�
=@�@ț�@��@�t�@�J@�?}@���@�t�@�@�?}@�V@��@�z�@��@�1@���@��;@�"�@���@�J@���@�hs@�V@��/@��@���@��-@���@��h@��@��P@�V@��@�?}@���@�dZ@�@�~�@��@��T@��T@���@�%@���@�1'@��;@��w@��F@��F@��@���@�t�@���@���@�=q@��#@��h@�?}@��@�Ĝ@���@��u@��@�Z@�b@�K�@��\@��@��@���@���@�=q@�b@�;d@�"�@�
=@��@��R@��+@�ff@�V@��@�@���@�7L@�%@��`@���@��9@���@�bN@�A�@�(�@�  @��w@��@���@���@�|�@�dZ@�S�@�K�@�K�@�"�@��@��+@�-@�@�@���@���@���@��D@� �@�1@��
@���@���@�33@��!@�M�@�J@��#@���@��7@�x�@�p�@�p�@�p�@�`B@�G�@��@���@���@�Z@�(�@�  @���@��m@��
@��@���@�t�@�dZ@�S�@�;d@�;d@�33@�33@�33@��@���@��y@���@���@�E�@���@���@��@�`B@�?}@�V@���@��;@��P@�;d@��y@��R@�v�@�M�@�$�@��^@���@�z�@��@�ƨ@��w@��w@��F@��F@���@�"�@���@��H@��R@�v�@�V@��@��7@�Ĝ@��9@���@���@��u@��D@�1@�l�@�o@��H@���@�^5@�V@�E�@�$�@��@�`B@�/@�V@���@���@��`@���@�Ĝ@���@�r�@�1'@���@�l�@�;d@��!@�n�@�5?@�@�p�@�%@���@�Ĝ@�Ĝ@��@���@�Q�@� �@��@��+@s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��mA��mA��A��yA��A��A��A��A��A��A��A�JA�JA�JA�A��A��A��;A�ȴAԮAԗ�Aԇ+A�t�A�ffA�ƨA��Aχ+A���AċDA¸RA��^A�I�A�
=A�1A�x�A��A��9A��wA��A�oA�+A� �A��!A���A��A��A�VA���A�oA�~�A�{A�t�A�dZA���A��A�33A�A��A�S�A��A��A���A�I�A���A��A�(�A�^5A���A�I�A��A��+A�  A���A���A���A�|�A�-A�E�A�E�A�5?A�$�A��A���A�^5A���A��A���A��A��A�1A��;A�r�A���A{/Ay�;Axv�Aw��AwdZAw�Av�AvffAt9XAn�jAn{Am��AmC�Al9XAh�HAe��Abn�A^JA[oAZ�+AX1'ASS�AQ�-AP��AN��AMx�AL�HAK��AKhsAKAI�TAH�AF1ADffAC\)AA��A@�A?��A>��A<�yA:�jA9|�A8VA6��A5A3XA3?}A3/A2ȴA2ffA1�PA/�hA-x�A-VA,ȴA,�\A,�A+K�A*r�A)�7A)l�A)S�A)&�A(�/A'�^A%�wA"ȴA!�A   AĜA"�A�hA5?A\)AS�A��A��A�A`BA(�AJAp�A�7A�A=qAXA\)A/A�AA
9XA�/A�jA�uAffAA�A(�A{A{A1A�mA��A�hAC�A�A��A�A��A�AbNAdZA�AI�At�A �@���@�M�@�`B@���@��-@�&�@�z�@��;@���@�b@�V@�j@���@���@�V@�"�@�h@�1'@�7L@�w@��y@�!@�$�@��`@߅@�=q@��T@�`B@ܼj@���@�&�@ؓu@�b@��;@׍P@֧�@Չ7@Ԭ@�b@ӍP@�@�n�@Гu@��@�~�@�V@�$�@��@Ͳ-@�O�@̴9@��@�
=@�@ț�@��@�t�@�J@�?}@���@�t�@�@�?}@�V@��@�z�@��@�1@���@��;@�"�@���@�J@���@�hs@�V@��/@��@���@��-@���@��h@��@��P@�V@��@�?}@���@�dZ@�@�~�@��@��T@��T@���@�%@���@�1'@��;@��w@��F@��F@��@���@�t�@���@���@�=q@��#@��h@�?}@��@�Ĝ@���@��u@��@�Z@�b@�K�@��\@��@��@���@���@�=q@�b@�;d@�"�@�
=@��@��R@��+@�ff@�V@��@�@���@�7L@�%@��`@���@��9@���@�bN@�A�@�(�@�  @��w@��@���@���@�|�@�dZ@�S�@�K�@�K�@�"�@��@��+@�-@�@�@���@���@���@��D@� �@�1@��
@���@���@�33@��!@�M�@�J@��#@���@��7@�x�@�p�@�p�@�p�@�`B@�G�@��@���@���@�Z@�(�@�  @���@��m@��
@��@���@�t�@�dZ@�S�@�;d@�;d@�33@�33@�33@��@���@��y@���@���@�E�@���@���@��@�`B@�?}@�V@���@��;@��P@�;d@��y@��R@�v�@�M�@�$�@��^@���@�z�@��@�ƨ@��w@��w@��F@��F@���@�"�@���@��H@��R@�v�@�V@��@��7@�Ĝ@��9@���@���@��u@��D@�1@�l�@�o@��H@���@�^5@�V@�E�@�$�@��@�`B@�/@�V@���@���@��`@���@�Ĝ@���@�r�@�1'@���@�l�@�;d@��!@�n�@�5?@�@�p�@�%@���@�Ĝ@�Ĝ@��@���@�Q�@� �@��@��+@s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�'B
�'B
�'B
�-B
�'B
�-B
�-B
�'B
�'B
�'B
�-B
�'B
�9B
��B+B%B�BXBv�B��B��B��B��BBBBBB+B{B&�B/BE�BM�BO�BO�BR�BS�BR�BR�BQ�BP�BN�BL�BJ�BF�BD�BC�BB�B@�B<jB9XB5?B1'B+B�B\BB��B�sB��BƨBÖB��B�^B�-B��B�oB�Bp�BhsB^5BR�BM�BH�B=qB0!B!�B�B\BB
�B
�HB
��B
ÖB
�}B
�LB
�B
��B
�B
|�B
t�B
n�B
YB
1'B
B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	ƨB	B	�dB	��B	�VB	o�B	Q�B	C�B	>wB	:^B	+B	%�B	!�B	�B	�B	oB	JB	
=B	+B	  B��B�B�B�sB�ZB�HB�BB�)B�
B��B��BɺBĜB��B�wB�wB�qB�jB�^B�LB�3B�'B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�DB�JB�PB��B��B��B��B�DB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�-B�3B�3B�9B�9B�?B�?B�FB�FB�RB�^B�dB�jB�jB�qB�wBĜBȴB��B��B��B��B��B��B��B��B�
B�B�/B�;B�HB�mB�B�B�B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	%B	%B	%B	bB	�B	�B	�B	�B	%�B	.B	0!B	33B	@�B	A�B	D�B	G�B	M�B	O�B	P�B	W
B	^5B	cTB	ffB	hsB	iyB	iyB	iyB	iyB	iyB	iyB	l�B	o�B	p�B	s�B	v�B	x�B	z�B	{�B	{�B	|�B	|�B	|�B	~�B	�B	�B	�+B	�1B	�JB	�DB	�7B	�%B	�B	�B	�B	�B	�%B	�7B	�=B	�DB	�PB	�PB	�\B	�bB	�hB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�9B	�FB	�RB	�^B	�^B	�dB	�dB	�dB	�jB	�jB	�qB	�wB	��B	B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�;B	�NB	�TB	�TB	�ZB	�ZB	�ZB	�TB	�NB	�TB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
1B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
JB
JB
PB
VB
VB
VB
VB
VB
VB
VB
\B
bB
bB
bB
bB
bB
hB
oB
oB
�B
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�'B
�'B
�'B
�-B
�'B
�-B
�-B
�'B
�'B
�'B
�-B
�'B
�9B
��B+B%B�BXBv�B��B��B��B��BBBBBB+B{B&�B/BE�BM�BO�BO�BR�BS�BR�BR�BQ�BP�BN�BL�BJ�BF�BD�BC�BB�B@�B<jB9XB5?B1'B+B�B\BB��B�sB��BƨBÖB��B�^B�-B��B�oB�Bp�BhsB^5BR�BM�BH�B=qB0!B!�B�B\BB
�B
�HB
��B
ÖB
�}B
�LB
�B
��B
�B
|�B
t�B
n�B
YB
1'B
B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	ƨB	B	�dB	��B	�VB	o�B	Q�B	C�B	>wB	:^B	+B	%�B	!�B	�B	�B	oB	JB	
=B	+B	  B��B�B�B�sB�ZB�HB�BB�)B�
B��B��BɺBĜB��B�wB�wB�qB�jB�^B�LB�3B�'B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�DB�JB�PB��B��B��B��B�DB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�-B�3B�3B�9B�9B�?B�?B�FB�FB�RB�^B�dB�jB�jB�qB�wBĜBȴB��B��B��B��B��B��B��B��B�
B�B�/B�;B�HB�mB�B�B�B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	%B	%B	%B	bB	�B	�B	�B	�B	%�B	.B	0!B	33B	@�B	A�B	D�B	G�B	M�B	O�B	P�B	W
B	^5B	cTB	ffB	hsB	iyB	iyB	iyB	iyB	iyB	iyB	l�B	o�B	p�B	s�B	v�B	x�B	z�B	{�B	{�B	|�B	|�B	|�B	~�B	�B	�B	�+B	�1B	�JB	�DB	�7B	�%B	�B	�B	�B	�B	�%B	�7B	�=B	�DB	�PB	�PB	�\B	�bB	�hB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�9B	�FB	�RB	�^B	�^B	�dB	�dB	�dB	�jB	�jB	�qB	�wB	��B	B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�;B	�NB	�TB	�TB	�ZB	�ZB	�ZB	�TB	�NB	�TB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
1B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
JB
JB
PB
VB
VB
VB
VB
VB
VB
VB
\B
bB
bB
bB
bB
bB
hB
oB
oB
�B
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140834                              AO  ARCAADJP                                                                    20181024140834    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140834  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140834  QCF$                G�O�G�O�G�O�0               