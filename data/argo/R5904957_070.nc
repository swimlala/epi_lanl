CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:17Z creation      
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
_FillValue                 �  ?�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  A�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  I�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  PD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  V�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Xl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ^�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  `�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  m�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  u�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  wh   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  }�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ~   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140817  20181024140817  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               FA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$�B�)1   @��%��̎@2;dZ��c��l�C�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      FA   A   A   @�  @�  A   A   A@  Aa��A�  A�  A�  A���A���A�  A�  A�  B   B��B  B  B   B(  B0  B8ffB@ffBH  BO��BX  B`  Bh  BpffBx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C�fC  C  C  C  C�fC  C  C  C   C"  C$  C&  C(�C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCe�fCh  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~�C��C�  C��C��C��C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C��3C�  C��3C�  C�  C��3C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��Dy�D  D� D  D� D  D�fD  D� DfD� D  D� D  D� D��D	y�D
  D
� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� DfD� D  D� D  D� DfD� D��D� D  Dy�D  D� D  D� D  D� D  D� D  Dy�D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6�fD7fD7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO�fDy��D�K�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��H@��HAp�A!p�AAp�Ac
>A��RA��RA��RA��A��AиRA�RA�RB \)B��B\)B\)B \)B(\)B0\)B8B@BH\)BO��BX\)B`\)Bh\)BpBx\)B�.B�aGB�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�aGB�.B�.B���B�.B�.B�.B�.B�.B�.B�.B�aGB�.B�.B�.B�aGB�.B�.B�.B�.C 
C
C
C
C
C

C
C�pC
C
C
C
C�pC
C
C
C 
C"
C$
C&
C(0�C*0�C,
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
CM�pCO�pCR
CT
CV
CX
CZ
C\
C^
C`
Cb
Cc�pCe�pCh
Cj0�Cl
Cn
Cp
Cr
Ct
Cv
Cx
Cz
C|0�C~0�C�RC��C�RC�RC�RC��C��C���C��C��C��C��C�RC�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C�RC��C�RC�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC�RC��C��C���C���C��C���C��C��C���C��C���C���C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C�RC��C��C�RC�RC�RC�RC��C��C��C��C��C��C��C��C��D �D ��D �]D]D�D��D�D��D�D�)D�D��D)D��D�D��D�D��D�]D	]D
�D
��D�D�)D�D��D�D��D�D��D�D��D�D��D�D��D�D]D�D��D�D��D)D��D�D��D�D��D)D��D�]D��D�D]D�D��D�D��D�D��D�D��D�D]D �D ��D!�D!��D"�D"��D#�D#��D$�D$�)D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*]D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6�)D7)D7��D8�D8��D9�D9��D:�D:��D;�D;]D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DG�]DH]DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO�)Dy�{D�N�D�Ǯ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�ĜA�ȴA���A�ȴA�ĜA�A�ĜA�A�ƨA�ȴA�ƨA���A���A���A���A�ȴA���A���A���A�ƨAݮAݍPA���A�C�A�bA��yA��AՋDA�
=A�$�A�/A��
A̬A�1'A���A�bAɗ�A�ffA�`BA�ĜA�G�A�AĸRA�ȴA�ĜA�{A���A�ĜA���A���A���A�%A��A�n�A���A�A�A�A�r�A��A��hA���A��jA�^5A��DA�O�A�=qA���A��wA�n�A�9XA�ƨA��A�`BA�5?A�9XA�|�A���A�hsA��HA�VA�ƨA�O�A��A��
A�\)A�oA�(�A�;dA�x�A��A�A�t�A�VA��TA���A�n�A���A�/A�1A�ffA��A���A���A��`A��A�mA}?}A{�Az�Ay\)Ax$�Av{Atv�Ar�Ao�wAn^5Am�Al�Ak�Ah�AfffAb�`Aap�A_p�A^�`A]hsA[��AZE�AX�yAW�^AWdZAW%AV��AU�AQ��AP�`APQ�AOl�AM|�AK7LAI\)AG7LAE��AD�AB�DA=�TA<Q�A:��A9��A7��A6�!A5hsA2�DA0M�A/�A.�\A.1A,�jA+�A+"�A*�+A*I�A)+A%�hA#��A"��A"9XA!7LA~�A33AXA�A��A�A�AhsA�A1AXAjAC�A�AJAdZAr�A�A��AVAJAG�AO�A�/AQ�AjA�7A;dA	ƨA�Av�A�A(�A��A��A�TA ��@�b@��@�S�@���@�/@���@��T@�K�@��T@� �@�C�@�
=@�33@@���@�?}@�@�z�@��
@�S�@�V@��@웦@�v�@�j@��@䛦@�K�@�=q@�Ĝ@�+@ݡ�@ܼj@�A�@��;@�l�@�
=@��@�7L@�7L@��@�Q�@� �@��
@�o@�ȴ@և+@��@��@�1@�ƨ@�33@θR@Η�@͙�@�t�@�
=@ʏ\@ə�@�V@�r�@ǅ@š�@�?}@��@�Ĝ@Ĭ@ċD@�Q�@�(�@�@��T@���@���@��7@�G�@��@�%@���@��j@�Z@�t�@���@�ff@��@���@�hs@�&�@��j@�j@�1'@�|�@���@�x�@�Ĝ@��@�9X@��;@��@�X@��/@���@�Ĝ@���@��@�1@��@�1@�(�@���@���@��R@�J@���@�V@�?}@��/@�Q�@��@��;@���@��H@�=q@�@�/@�A�@�l�@�"�@�"�@��@��@�ȴ@��\@�v�@�ff@��#@�x�@�?}@��@���@���@�j@�(�@��m@��w@�|�@�33@��R@�~�@�ff@�V@�-@���@��7@���@���@�  @�33@���@�$�@���@���@��h@��7@��`@�1'@� �@��@��P@��@�t�@��@�o@���@��+@�~�@�V@�hs@�/@��@�Ĝ@�bN@�b@���@���@�bN@��@�C�@���@�=q@���@���@��@���@�J@�$�@�-@�5?@��T@��u@���@�S�@��@���@���@��\@�-@���@q`B@Z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�A�ĜA�ȴA���A�ȴA�ĜA�A�ĜA�A�ƨA�ȴA�ƨA���A���A���A���A�ȴA���A���A���A�ƨAݮAݍPA���A�C�A�bA��yA��AՋDA�
=A�$�A�/A��
A̬A�1'A���A�bAɗ�A�ffA�`BA�ĜA�G�A�AĸRA�ȴA�ĜA�{A���A�ĜA���A���A���A�%A��A�n�A���A�A�A�A�r�A��A��hA���A��jA�^5A��DA�O�A�=qA���A��wA�n�A�9XA�ƨA��A�`BA�5?A�9XA�|�A���A�hsA��HA�VA�ƨA�O�A��A��
A�\)A�oA�(�A�;dA�x�A��A�A�t�A�VA��TA���A�n�A���A�/A�1A�ffA��A���A���A��`A��A�mA}?}A{�Az�Ay\)Ax$�Av{Atv�Ar�Ao�wAn^5Am�Al�Ak�Ah�AfffAb�`Aap�A_p�A^�`A]hsA[��AZE�AX�yAW�^AWdZAW%AV��AU�AQ��AP�`APQ�AOl�AM|�AK7LAI\)AG7LAE��AD�AB�DA=�TA<Q�A:��A9��A7��A6�!A5hsA2�DA0M�A/�A.�\A.1A,�jA+�A+"�A*�+A*I�A)+A%�hA#��A"��A"9XA!7LA~�A33AXA�A��A�A�AhsA�A1AXAjAC�A�AJAdZAr�A�A��AVAJAG�AO�A�/AQ�AjA�7A;dA	ƨA�Av�A�A(�A��A��A�TA ��@�b@��@�S�@���@�/@���@��T@�K�@��T@� �@�C�@�
=@�33@@���@�?}@�@�z�@��
@�S�@�V@��@웦@�v�@�j@��@䛦@�K�@�=q@�Ĝ@�+@ݡ�@ܼj@�A�@��;@�l�@�
=@��@�7L@�7L@��@�Q�@� �@��
@�o@�ȴ@և+@��@��@�1@�ƨ@�33@θR@Η�@͙�@�t�@�
=@ʏ\@ə�@�V@�r�@ǅ@š�@�?}@��@�Ĝ@Ĭ@ċD@�Q�@�(�@�@��T@���@���@��7@�G�@��@�%@���@��j@�Z@�t�@���@�ff@��@���@�hs@�&�@��j@�j@�1'@�|�@���@�x�@�Ĝ@��@�9X@��;@��@�X@��/@���@�Ĝ@���@��@�1@��@�1@�(�@���@���@��R@�J@���@�V@�?}@��/@�Q�@��@��;@���@��H@�=q@�@�/@�A�@�l�@�"�@�"�@��@��@�ȴ@��\@�v�@�ff@��#@�x�@�?}@��@���@���@�j@�(�@��m@��w@�|�@�33@��R@�~�@�ff@�V@�-@���@��7@���@���@�  @�33@���@�$�@���@���@��h@��7@��`@�1'@� �@��@��P@��@�t�@��@�o@���@��+@�~�@�V@�hs@�/@��@�Ĝ@�bN@�b@���@���@�bN@��@�C�@���@�=q@���@���@��@���@�J@�$�@�-@�5?@��T@��u@���@�S�@��@���@���@��\@�-@���@q`B@Z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�BB
�BB
�;B
�;B
�;B
�;B
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�;B
�;B
�;B
�;B
�;B
�;B
�BB
�BB
�;B
�/B
�)B
�BjB�\B�DB�1B�%B��B��B�^B��B��B%B$�B/B9XBH�BXB� B�bB��B��B��B�B�qB�qB��B��B��B�;B�sB�HB�/B�#B�)B�B��B��BɺB�RB��B�#B��B�B�B�sB�HB��B�?B�B��B�1Bp�BW
BF�B;dB(�BuB	7BB��B�B�TB�#BŢB��BhsBK�B2-B!�B�B	7B
�B
�ZB
�HB
�#B
�B
ɺB
�?B
��B
�oB
�1B
u�B
]/B
K�B
9XB
.B
#�B
�B
hB
B	��B	�B	�/B	��B	��B	ĜB	�qB	�B	��B	�1B	}�B	t�B	p�B	gmB	`BB	YB	P�B	J�B	H�B	E�B	A�B	:^B	(�B	#�B	 �B	�B	\B	B��B�B�B�yB�/B��BɺBÖB�qB�dB�jB�XB�3B�B��B��B��B��B��B��B��B��B��B��B�hB�VB�PB�PB�7B�Bu�Bo�Br�Bq�Bu�B�B�bB��B�B�B�B��B��B��B��B�'B�FB�jB��B�B��B��B�B�NB�B��B�-B�JB�B�B�B~�Bz�Bv�Bq�Bm�Bq�Bw�B�B�=B�7B~�Bu�Br�Bx�B|�B� B�1B|�B��B�B�B�B�B�B�B�B�B�B�!B�3B�3B�9B�3B�?B�LB�dB�qB�qB�wB�wB�wBBĜBĜBÖBŢBŢBƨBȴBȴBǮB��B��B�B�B�B�#B�#B�)B�TB�ZB�`B�sB�B�B�B��B��B��B	B	B	B	B	%B	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	$�B	&�B	&�B	'�B	)�B	,B	.B	2-B	2-B	6FB	:^B	;dB	:^B	<jB	>wB	?}B	@�B	@�B	@�B	@�B	A�B	A�B	D�B	F�B	F�B	J�B	N�B	M�B	P�B	R�B	VB	YB	[#B	ZB	ZB	YB	ZB	^5B	e`B	e`B	hsB	l�B	p�B	r�B	r�B	r�B	t�B	u�B	x�B	y�B	|�B	}�B	� B	�B	�B	�B	�%B	�+B	�7B	�DB	�DB	�PB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�'B	�'B	�'B	�3B	�?B	�LB	�RB	��B	��B	ÖB	ÖB	ÖB	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�/B	�NB	�HB	�BB	�5B	�/B	�5B	�;B	�TB	�`B	�fB	�fB	�fB	�mB	�mB	�ZB	�TB	�`B	�B	�B	�B	�B	�B
�B
,�B
=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�BB
�BB
�;B
�;B
�;B
�;B
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�;B
�;B
�;B
�;B
�;B
�;B
�BB
�BB
�;B
�/B
�)B
�BjB�\B�DB�1B�%B��B��B�^B��B��B%B$�B/B9XBH�BXB� B�bB��B��B��B�B�qB�qB��B��B��B�;B�sB�HB�/B�#B�)B�B��B��BɺB�RB��B�#B��B�B�B�sB�HB��B�?B�B��B�1Bp�BW
BF�B;dB(�BuB	7BB��B�B�TB�#BŢB��BhsBK�B2-B!�B�B	7B
�B
�ZB
�HB
�#B
�B
ɺB
�?B
��B
�oB
�1B
u�B
]/B
K�B
9XB
.B
#�B
�B
hB
B	��B	�B	�/B	��B	��B	ĜB	�qB	�B	��B	�1B	}�B	t�B	p�B	gmB	`BB	YB	P�B	J�B	H�B	E�B	A�B	:^B	(�B	#�B	 �B	�B	\B	B��B�B�B�yB�/B��BɺBÖB�qB�dB�jB�XB�3B�B��B��B��B��B��B��B��B��B��B��B�hB�VB�PB�PB�7B�Bu�Bo�Br�Bq�Bu�B�B�bB��B�B�B�B��B��B��B��B�'B�FB�jB��B�B��B��B�B�NB�B��B�-B�JB�B�B�B~�Bz�Bv�Bq�Bm�Bq�Bw�B�B�=B�7B~�Bu�Br�Bx�B|�B� B�1B|�B��B�B�B�B�B�B�B�B�B�B�!B�3B�3B�9B�3B�?B�LB�dB�qB�qB�wB�wB�wBBĜBĜBÖBŢBŢBƨBȴBȴBǮB��B��B�B�B�B�#B�#B�)B�TB�ZB�`B�sB�B�B�B��B��B��B	B	B	B	B	%B	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	$�B	&�B	&�B	'�B	)�B	,B	.B	2-B	2-B	6FB	:^B	;dB	:^B	<jB	>wB	?}B	@�B	@�B	@�B	@�B	A�B	A�B	D�B	F�B	F�B	J�B	N�B	M�B	P�B	R�B	VB	YB	[#B	ZB	ZB	YB	ZB	^5B	e`B	e`B	hsB	l�B	p�B	r�B	r�B	r�B	t�B	u�B	x�B	y�B	|�B	}�B	� B	�B	�B	�B	�%B	�+B	�7B	�DB	�DB	�PB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�'B	�'B	�'B	�3B	�?B	�LB	�RB	��B	��B	ÖB	ÖB	ÖB	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�/B	�NB	�HB	�BB	�5B	�/B	�5B	�;B	�TB	�`B	�fB	�fB	�fB	�mB	�mB	�ZB	�TB	�`B	�B	�B	�B	�B	�B
�B
,�B
=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140817                              AO  ARCAADJP                                                                    20181024140817    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140817  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140817  QCF$                G�O�G�O�G�O�0               