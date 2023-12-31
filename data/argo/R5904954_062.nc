CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:02Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191702  20181005191702  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               >A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׾�S�>1   @׾噙�`@5]�E���dz�G�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      >A   A   A   @�33@�  A   A   A@  A`  A~ffA�33A�33A���A���A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BG��BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CC�fCF  CH  CJ  CL  CN  CP  CR�CT�CV  CW�fCZ  C\�C^  C`  Cb�Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr�Ct  Cu�fCx  Cz�C|  C~  C��C�  C��3C��C��C�  C��3C�  C��C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C��C�  C��C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C��3C��C��3C��C�  C�  C�  C��3C��3C��3C��C�  C�  C�  C��3C�  C��C��C��C��3C��fC��3C�  C�  C��C�  C��3C��3C��C�  C��3C�  C�  C��3C��C��C��C�  C�  C��3C�  C��C�  C�  C�  C��C��C��fC��3C��fC��3C�  C�  C��C��C�  C�  C��C��C�  C��3C��3C�  C�  C�  C�  C��3C�  C��C��C��C�  C��3C��3C�  C�  C�  C��C��D fD �fD  D�fDfD�fD�D�fD��D� DfD� D��D� D  D�fDfD�fD	  D	�fD
fD
�fDfD�fDfD�fD  D� D  D� D  Dy�D��Dy�DfD� D��D� D  Dy�D  D� D�3Dy�D  D�fD  D� D  D� D  D� D�D� D  D�fDfD�fDfD�fD  D�fDfDy�D��D � D!  D!� D"fD"�fD#  D#� D$  D$y�D%  D%y�D%��D&y�D&��D'y�D(fD(� D)fD)� D)��D*y�D+  D+� D,  D,� D-  D-� D-��D.� D.��D/y�D/��D0y�D1  D1�fD2  D2y�D3  D3�fD4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9y�D9��D:y�D;fD;� D;��D<� D=fD=�fD>  D>y�D?  D?� D@  D@�fDAfDA� DB  DBy�DB��DC�fDD  DDy�DE  DE�fDFfDF� DGfDG�fDH  DHy�DH��DIy�DJ  DJ�fDK  DKy�DK��DL� DM  DM� DNfDN�fDO  DOy�DP  DP� DP��DQ� DRfDR� DS  DS� DT  DT� DU  DU�fDVfDV� DW  DWy�DX  DX�fDY  DY�fDZ  DZ� D[fD[y�D\  D\�fD]fD]� D^  D^� D_  D_� D`fD`�fDa  Da� DbfDb�fDb��Dc� Dd  Dd� DefDe�fDf  Dfy�Df��Dg� DhfDh� Di  Di�fDjfDj�fDk  Dk� DlfDly�Dl��Dmy�Dn  Dny�Do  Do� Dp  Dpy�Dq  Dq� Dq��Dr� Dr��Ds� Dt  Dts3Du  Du� Du��Dv�fDw  Dwy�Dw��Dy}qD�G�D�v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@��HAp�A!p�AAp�Aap�A�
A��A��A��A��AиRA�RA�RB \)B\)B\)B\)B \)B'��B0\)B8\)B@\)BG��BP\)BX\)B_��Bh\)Bp\)Bx\)B�.B�.B�.B�.B�aGB�.B�.B�.B�.B���B�.B�.B�.B�.B�.B�.B���B���B�.B�.B�aGB�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C 
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
C�pC
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
C@0�CB
CC�pCF
CH
CJ
CL
CN
CP
CR0�CT0�CV
CW�pCZ
C\0�C^
C`
Cb0�Cd
Ce�pCh
Cj
Cl
Cn
Cp
Cr0�Ct
Cu�pCx
Cz0�C|
C~
C�RC��C���C�RC�RC��C���C��C�RC��C��C��C�RC��C���C��C��C���C��C��C�RC��C�RC��C�RC��C��C��C��C�RC��C��C��C�RC���C��C��C��C��C��C���C��C��C���C��C�RC��C��C��C���C�RC���C�RC��C��C��C���C���C���C�%C��C��C��C���C��C�RC�RC�RC���C���C���C��C��C�RC��C���C���C�RC��C���C��C��C���C�RC�%C�RC��C��C���C��C�RC��C��C��C�RC�RC���C���C���C���C��C��C�RC�%C��C��C�RC�RC��C���C���C��C��C��C��C���C��C�%C�RC�RC��C���C���C��C��C��C�%C�%D )D �)D�D�)D)D�)D�D�)D�]D��D)D��D�]D��D�D�)D)D�)D	�D	�)D
)D
�)D)D�)D)D�)D�D��D�D��D�D]D�]D]D)D��D�]D��D�D]D�D��D��D]D�D�)D�D��D�D��D�D��D�D��D�D�)D)D�)D)D�)D�D�)D)D]D�]D ��D!�D!��D")D"�)D#�D#��D$�D$]D%�D%]D%�]D&]D&�]D']D()D(��D))D)��D)�]D*]D+�D+��D,�D,��D-�D-��D-�]D.��D.�]D/]D/�]D0]D1�D1�)D2�D2]D3�D3�)D4)D4��D5�D5��D6�D6��D7�D7��D8�D8��D8�]D9]D9�]D:]D;)D;��D;�]D<��D=)D=�)D>�D>]D?�D?��D@�D@�)DA)DA��DB�DB]DB�]DC�)DD�DD]DE�DE�)DF)DF��DG)DG�)DH�DH]DH�]DI]DJ�DJ�)DK�DK]DK�]DL��DM�DM��DN)DN�)DO�DO]DP�DP��DP�]DQ��DR)DR��DS�DS��DT�DT��DU�DU�)DV)DV��DW�DW]DX�DX�)DY�DY�)DZ�DZ��D[)D[]D\�D\�)D])D]��D^�D^��D_�D_��D`)D`�)Da�Da��Db)Db�)Db�]Dc��Dd�Dd��De)De�)Df�Df]Df�]Dg��Dh)Dh��Di�Di�)Dj)Dj�)Dk�Dk��Dl)Dl]Dl�]Dm]Dn�Dn]Do�Do��Dp�Dp]Dq�Dq��Dq�]Dr��Dr�]Ds��Dt�Dtx�Du�Du��Du�]Dv�)Dw�Dw]DwҐDy�4D�J�D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�%A�
=A�1A�1A�1A�JA�bA�oA��A���A�=qA���A��A��A��A��A�A�n�A�A�ȴA��Aʥ�A�t�A�(�Aɰ!A�ȴA��Aŕ�A�ffA�1A�I�A��mAÇ+A��mA��A�+A�XA��hA�JA���A�=qA��A�G�A�K�A��TA�x�A�1'A���A��A���A���A��A�`BA�C�A���A�^5A���A�  A���A��!A���A�VA�A�A�&�A�  A�bNA���A�{A��wA�-A��A��jA�S�A��A�dZA�n�A��;A�XA��TA��7A���A�XA��A��\A��A�O�A��DA��hA���A���A���A���A��\A��DA�~�A���A�$�A���A�~�A���A�/AhsA{�Azn�Az{AyoAw&�Atz�Ap{An��Alr�AkVAip�AhĜAg�;AfjAd��Ac`BAbz�Aa`BA_K�AZ�HAX-AW��AWXAV�/AU��AT��ARAN�HAL�AJ��AG�
AD~�AA��A?�^A>r�A:��A9l�A8�jA7O�A5�A5VA4��A49XA2�A1&�A-ƨA+�hA+A*^5A)33A(A�A&�\A$jA"�DA!��A!+A �RAl�A9XA~�AS�Az�AXA�A�A�-A�#A�A7LA�9A&�A��AE�A��A�A�AdZA7LAO�A
�jA
��A
M�A	�A	�A	ƨA	%A~�A�jAoA�A$�A�
A�wA��At�AO�A7LA�yA�TA+A �/A ��A �RA 1'@��/@���@�  @�o@�+@�33@���@���@�|�@��@��#@�l�@��R@��^@��;@��@�ȴ@�/@�9@�ȴ@�1'@�w@�C�@��#@��@��y@�-@�?}@�z�@�\)@�^5@��@�%@� �@ߝ�@�t�@�|�@޸R@��#@�x�@��@��@��@���@�\)@֏\@�{@�O�@�%@ԋD@�33@ҏ\@�ff@�M�@�5?@�{@�`B@Ь@��@Ϯ@�C�@�
=@��@�^5@�5?@���@�V@�r�@�1'@��m@��y@�E�@�hs@�%@��`@�Z@Ǯ@�33@��@���@���@��@�&�@��/@��j@�9X@�l�@���@�-@�&�@�  @���@���@���@��m@��m@�ƨ@���@�l�@��@��+@�M�@�E�@�J@��@�(�@��F@�C�@���@���@�^5@��@��^@��h@�`B@�7L@��`@�9X@��@��!@�V@��#@�p�@�7L@��@��9@�1@�A�@��@��P@�C�@�"�@�
=@��y@��y@��H@��R@�n�@�5?@�@���@�O�@�&�@�Ĝ@� �@��@�"�@���@���@�5?@��h@�Q�@��P@�o@���@�v�@�-@�@��@���@�Ĝ@�r�@�Q�@�b@�(�@�(�@��@�1@���@���@���@���@���@�K�@�@��y@���@�@�@��7@�?}@��@�X@�V@���@���@�+@��@��!@�ff@�=q@�$�@���@��@�=q@�^5@�@��7@�O�@��@��/@���@�bN@��;@��F@�;d@��\@�E�@�{@��-@�p�@�hs@�O�@�7L@���@�j@�(�@�b@��@��m@��;@��
@���@���@��@��P@�K�@�;d@�"�@��H@�ȴ@�$�@��T@���@���@��-@��7@�`B@��/@��@�z�@�I�@�(�@��
@��@�C�@���@��w@�|�@��H@�$�@���@��7@���@�X@�x�@�x�@��@�j@�ƨ@�;d@���@��\@���@���@��+@���@��T@���@��^@�&�@��@��@�Q�@��@���@��!@��+@�E�@��@�J@��@��#@���@�O�@��`@���@��@���@{)_@h9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�%A�
=A�1A�1A�1A�JA�bA�oA��A���A�=qA���A��A��A��A��A�A�n�A�A�ȴA��Aʥ�A�t�A�(�Aɰ!A�ȴA��Aŕ�A�ffA�1A�I�A��mAÇ+A��mA��A�+A�XA��hA�JA���A�=qA��A�G�A�K�A��TA�x�A�1'A���A��A���A���A��A�`BA�C�A���A�^5A���A�  A���A��!A���A�VA�A�A�&�A�  A�bNA���A�{A��wA�-A��A��jA�S�A��A�dZA�n�A��;A�XA��TA��7A���A�XA��A��\A��A�O�A��DA��hA���A���A���A���A��\A��DA�~�A���A�$�A���A�~�A���A�/AhsA{�Azn�Az{AyoAw&�Atz�Ap{An��Alr�AkVAip�AhĜAg�;AfjAd��Ac`BAbz�Aa`BA_K�AZ�HAX-AW��AWXAV�/AU��AT��ARAN�HAL�AJ��AG�
AD~�AA��A?�^A>r�A:��A9l�A8�jA7O�A5�A5VA4��A49XA2�A1&�A-ƨA+�hA+A*^5A)33A(A�A&�\A$jA"�DA!��A!+A �RAl�A9XA~�AS�Az�AXA�A�A�-A�#A�A7LA�9A&�A��AE�A��A�A�AdZA7LAO�A
�jA
��A
M�A	�A	�A	ƨA	%A~�A�jAoA�A$�A�
A�wA��At�AO�A7LA�yA�TA+A �/A ��A �RA 1'@��/@���@�  @�o@�+@�33@���@���@�|�@��@��#@�l�@��R@��^@��;@��@�ȴ@�/@�9@�ȴ@�1'@�w@�C�@��#@��@��y@�-@�?}@�z�@�\)@�^5@��@�%@� �@ߝ�@�t�@�|�@޸R@��#@�x�@��@��@��@���@�\)@֏\@�{@�O�@�%@ԋD@�33@ҏ\@�ff@�M�@�5?@�{@�`B@Ь@��@Ϯ@�C�@�
=@��@�^5@�5?@���@�V@�r�@�1'@��m@��y@�E�@�hs@�%@��`@�Z@Ǯ@�33@��@���@���@��@�&�@��/@��j@�9X@�l�@���@�-@�&�@�  @���@���@���@��m@��m@�ƨ@���@�l�@��@��+@�M�@�E�@�J@��@�(�@��F@�C�@���@���@�^5@��@��^@��h@�`B@�7L@��`@�9X@��@��!@�V@��#@�p�@�7L@��@��9@�1@�A�@��@��P@�C�@�"�@�
=@��y@��y@��H@��R@�n�@�5?@�@���@�O�@�&�@�Ĝ@� �@��@�"�@���@���@�5?@��h@�Q�@��P@�o@���@�v�@�-@�@��@���@�Ĝ@�r�@�Q�@�b@�(�@�(�@��@�1@���@���@���@���@���@�K�@�@��y@���@�@�@��7@�?}@��@�X@�V@���@���@�+@��@��!@�ff@�=q@�$�@���@��@�=q@�^5@�@��7@�O�@��@��/@���@�bN@��;@��F@�;d@��\@�E�@�{@��-@�p�@�hs@�O�@�7L@���@�j@�(�@�b@��@��m@��;@��
@���@���@��@��P@�K�@�;d@�"�@��H@�ȴ@�$�@��T@���@���@��-@��7@�`B@��/@��@�z�@�I�@�(�@��
@��@�C�@���@��w@�|�@��H@�$�@���@��7@���@�X@�x�@�x�@��@�j@�ƨ@�;d@���@��\@���@���@��+@���@��T@���@��^@�&�@��@��@�Q�@��@���@��!@��+@�E�@��@�J@��@��#@���@�O�@��`@���@��@���@{)_@h9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�^B�^B�^B�^B�^B�^B�^B�^B�XB�XB�FB�B�9B�^B�XB�dB�jB�qB�}B�BBoB�B�B�B�B,B1'B1'B1'B2-B;dB@�BF�BN�BW
B`BBgmBn�Br�Bv�B~�B�+B�PB�uB��B��B�B�-B�9B�9B�3B�!B�9B�jB�RB�^B�^B��B�JBt�Bl�Bm�Bl�Bk�BjBhsBgmBjBm�BiyBe`BcTB^5BO�B>wB2-B+B#�BPB�B�9B�{B�+Bu�BN�B0!BhB
�sB
�B
�B
�B
�B
�B
�B
�
B
��B
ĜB
�jB
�B
��B
��B
|�B
M�B
D�B
?}B
49B
"�B
\B	�B	�TB	��B	ǮB	�dB	�FB	�B	��B	��B	�hB	�=B	�B	u�B	_;B	P�B	L�B	J�B	E�B	?}B	6FB	$�B	hB	+B��B�B�fB�5B�B��B��BȴBȴBŢB�}B�jB�jB�XB�3B�XB�B��B��B��B��B�hB�=B�B�B�B�B� B� B}�B{�By�Bx�Bv�Bu�Bs�Br�Bo�Bk�BjBffBbNB_;B[#B]/B`BBaHB`BB_;B_;B^5B^5B^5B]/B]/B[#B[#BZB\)B^5B_;BaHBcTBe`BgmBjBo�Br�By�B�B�1B�=B�=B�7B�7B�DB�DB�PB�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B�B�B�'B�3B�RB�XB�XB�dB�jB�}B�wB�}B��B�}B��BÖBƨBƨBƨBȴBȴBɺB��B��B��B��B��B��B��B��B��B�B�
B�
B�B�B�#B�/B�BB�HB�HB�NB�`B�fB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B		7B	VB	oB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	!�B	'�B	,B	.B	0!B	2-B	33B	6FB	8RB	:^B	;dB	<jB	<jB	?}B	C�B	J�B	L�B	N�B	P�B	S�B	T�B	VB	YB	_;B	bNB	ffB	jB	m�B	n�B	p�B	s�B	t�B	t�B	w�B	}�B	� B	�B	�1B	�=B	�DB	�JB	�JB	�JB	�PB	�VB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�3B	�?B	�XB	�jB	�wB	�wB	�}B	�}B	�}B	�}B	�}B	�}B	��B	B	ÖB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�ZB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B
  B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
+B
�B
eB
)�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�^B�^B�^B�^B�^B�^B�^B�^B�XB�XB�FB�B�9B�^B�XB�dB�jB�qB�}B�BBoB�B�B�B�B,B1'B1'B1'B2-B;dB@�BF�BN�BW
B`BBgmBn�Br�Bv�B~�B�+B�PB�uB��B��B�B�-B�9B�9B�3B�!B�9B�jB�RB�^B�^B��B�JBt�Bl�Bm�Bl�Bk�BjBhsBgmBjBm�BiyBe`BcTB^5BO�B>wB2-B+B#�BPB�B�9B�{B�+Bu�BN�B0!BhB
�sB
�B
�B
�B
�B
�B
�B
�
B
��B
ĜB
�jB
�B
��B
��B
|�B
M�B
D�B
?}B
49B
"�B
\B	�B	�TB	��B	ǮB	�dB	�FB	�B	��B	��B	�hB	�=B	�B	u�B	_;B	P�B	L�B	J�B	E�B	?}B	6FB	$�B	hB	+B��B�B�fB�5B�B��B��BȴBȴBŢB�}B�jB�jB�XB�3B�XB�B��B��B��B��B�hB�=B�B�B�B�B� B� B}�B{�By�Bx�Bv�Bu�Bs�Br�Bo�Bk�BjBffBbNB_;B[#B]/B`BBaHB`BB_;B_;B^5B^5B^5B]/B]/B[#B[#BZB\)B^5B_;BaHBcTBe`BgmBjBo�Br�By�B�B�1B�=B�=B�7B�7B�DB�DB�PB�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B�B�B�'B�3B�RB�XB�XB�dB�jB�}B�wB�}B��B�}B��BÖBƨBƨBƨBȴBȴBɺB��B��B��B��B��B��B��B��B��B�B�
B�
B�B�B�#B�/B�BB�HB�HB�NB�`B�fB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B		7B	VB	oB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	!�B	'�B	,B	.B	0!B	2-B	33B	6FB	8RB	:^B	;dB	<jB	<jB	?}B	C�B	J�B	L�B	N�B	P�B	S�B	T�B	VB	YB	_;B	bNB	ffB	jB	m�B	n�B	p�B	s�B	t�B	t�B	w�B	}�B	� B	�B	�1B	�=B	�DB	�JB	�JB	�JB	�PB	�VB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�3B	�?B	�XB	�jB	�wB	�wB	�}B	�}B	�}B	�}B	�}B	�}B	��B	B	ÖB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�ZB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B
  B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
+B
�B
eB
)�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191702                              AO  ARCAADJP                                                                    20181005191702    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191702  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191702  QCF$                G�O�G�O�G�O�8000            