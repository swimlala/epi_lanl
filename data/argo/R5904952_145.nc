CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:37Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190537  20181005190537  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�ݩs�1   @�ݪ�|@0�fffff�c݉7Kƨ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  @���A   A@  A`  A���A���A���A�  A�  A�  A�  A�  B   B  BffB  B   B(ffB0ffB8  B@  BHffBP  BX  B`  Bg��Bp  BxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�33B�33B�33B�  B�  B���B���B���B�  B�33B�  B�  B�  B�33B�33B�  B�  C   C  C  C  C  C
  C  C�C  C33C��C�fC�fC�fC�fC  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD� D  Dy�D��D� DfD� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D��D� D  D�fDfD�fDfD� D��Dy�D��Dy�D  D�fDfD�fD  D� D  D�fD  D� D  D� D  D� D  D� D  Dy�D  D� DfD� D��D � D!  D!� D"  D"y�D"��D#� D$fD$� D%  D%� D&  D&y�D&��D'y�D'��D(� D)  D)� D*fD*� D+  D+�fD,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D0��D1y�D2  D2� D3  D3� D4  D4�fD5fD5� D6  D6� D7  D7�fD8fD8� D9  D9�fD:  D:y�D:��D;� D<fD<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DG� DH  DH� DI  DIy�DI��DJ� DKfDK� DL  DL� DM  DMy�DM��DNy�DN��DOy�DO��DP� DQ  DQ� DR  DR�fDS  DSy�DS��DT� DU  DU� DV  DV� DWfDW� DXfDX�fDYfDY� DZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^�fD_  D_y�D_��D`y�D`��Da� Db  Db� Dc  Dc� DdfDd�fDefDe�fDffDf�fDgfDg�fDh  Dh�fDifDi�fDj  Dj� Dk  Dk� Dl  Dl� Dl��Dmy�Dn  Dn�fDofDo� Dp  Dp� Dq  Dq� DrfDr� Ds  Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dw� Dw��Dy��D�HRD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��]@���@�A z�A@z�A`z�A�
>A�
>A�
>A�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bg�RBp�Bx�B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�\B��)B�\B�\B�B�B�B�B�B�B�\B�\B��)B��)B��)B�\B�B�B�\B�\B�\B�B�B�B�B�\B�\C �C�C�C�C�C
�C�C!HC�C:�C�{C�C�C�C�C�C �C"�C$�C&�C(�C*!HC,�C.�C0�C2�C4�C6�C8�C:�C<�C=�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ!HC\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��
C��
C��
C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��
C��
C��C��
C��
C��C��C��C��C��C��C��C��D �D ��D�D��D�D��DRD��D�D{�D��D��DRD��D�D��D�D��D	�D	��D
�D
�RD�D��D�D��D�D��D�D��D��D��D�D�RDRD�RDRD��D��D{�D��D{�D�D�RDRD�RD�D��D�D�RD�D��D�D��D�D��D�D��D�D{�D�D��DRD��D��D ��D!�D!��D"�D"{�D"��D#��D$RD$��D%�D%��D&�D&{�D&��D'{�D'��D(��D)�D)��D*RD*��D+�D+�RD,�D,��D-�D-{�D.�D.��D/�D/��D0�D0��D0��D1{�D2�D2��D3�D3��D4�D4�RD5RD5��D6�D6��D7�D7�RD8RD8��D9�D9�RD:�D:{�D:��D;��D<RD<��D=�D=��D>�D>��D?�D?��D@�D@{�DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DF��DG��DH�DH��DI�DI{�DI��DJ��DKRDK��DL�DL��DM�DM{�DM��DN{�DN��DO{�DO��DP��DQ�DQ��DR�DR�RDS�DS{�DS��DT��DU�DU��DV�DV��DWRDW��DXRDX�RDYRDY��DZ�DZ{�D[�D[��D\�D\��D]�D]��D^�D^�RD_�D_{�D_��D`{�D`��Da��Db�Db��Dc�Dc��DdRDd�RDeRDe�RDfRDf�RDgRDg�RDh�Dh�RDiRDi�RDj�Dj��Dk�Dk��Dl�Dl��Dl��Dm{�Dn�Dn�RDoRDo��Dp�Dp��Dq�Dq��DrRDr��Ds�Ds��Dt�Dt��Du�Du�RDv�Dv��Dw�Dw��Dw�Dy�{D�IHD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�ffA�jA�l�A�l�A�p�A�p�A�p�A�r�A�r�A�r�A�r�A�x�A�z�A�z�AуAсA�z�A�p�A�|�AуAщ7Aщ7AхAуAуAхAч+Aч+AыDAэPAэPAыDAэPAыDAэPAэPAя\AѓuAѓuAѕ�Aѕ�Aї�Aљ�Aљ�Aћ�Aћ�Aћ�Aћ�Aѝ�Aћ�Aѕ�AэPAэPAёhA�t�A�^5A�E�A�{A��;Aк^A�`BA�$�A�oA�A���A���Aϛ�A�^5A��TAΧ�A��HAȟ�Aę�A�jA��uA��RA��+A��A��wA��wA���A�1'A��9A���A��7A�S�A�$�A���A��yA�|�A�dZA��A�\)A�S�A���A��/A�(�A�ȴA�{A���A�+A��A��A�A�C�A�x�A�
=A�I�A�Q�A�JA�^5A���A�jA�;dA�ĜA��A�p�A�n�A�A���Ax(�At��Aq`BAo�FAm�-Ah�A`(�A]�AZz�AU�AQ7LAO"�AK�wAH1'AF�AC�-A@�uA>r�A=�
A<�A;�hA;�A:�yA:jA8�A7%A6A4~�A3\)A21A0v�A/��A/hsA.�A-�A+"�A*$�A)A(Q�A'�wA&�A%�-A$E�A#��A#��A#%A!&�A��AAA�A�hAM�A�uA�hA�jAbA/A�FA��A��Ar�A+A�A�FA�AffA��A$�A�TA��A+A��A��AffA1A�TA��A
ZA	t�A~�A$�AA�A�A�;A"�A�HAJA�/A�Al�A��AE�AffA r�@�^5@�O�@�Z@� �@ߥ�@�"�@��@ܬ@��@�(�@؃@�(�@�\)@�5?@ԓu@���@�@�~�@��#@�p�@��@��@��@��H@�\)@��y@�^5@ͩ�@�/@̛�@��;@˕�@�dZ@�"�@ʟ�@�J@ɩ�@ɑh@�O�@�/@���@�Z@�9X@�9X@�1'@ǅ@�v�@��@�J@�x�@��/@�r�@��
@�K�@�^5@�7L@��9@���@� �@��w@�33@���@�E�@�J@�x�@��9@��@�ȴ@�ȴ@��@�=q@��7@��@�V@���@���@��D@�(�@�S�@��#@��T@�hs@���@���@���@�%@��D@��@�l�@�@�5?@��T@�@�/@�V@���@��9@�9X@� �@�b@��@�t�@���@�K�@�t�@�;d@��H@�{@�@���@���@��h@��h@�x�@�X@���@��@���@���@�+@��\@��#@�p�@���@�I�@�1@��@��m@��@��P@�dZ@�"�@�o@�@��@��+@�J@��7@�X@�&�@��/@�j@�  @��;@�dZ@�
=@���@���@�{@�@��@�X@��@���@��@���@��@�"�@�C�@�dZ@���@�|�@�l�@�t�@�t�@�C�@�@��@��@���@��+@�$�@�x�@��@��D@�bN@�1'@�b@���@���@�C�@��H@���@�v�@�M�@�{@���@��7@�hs@�X@�7L@���@� �@���@��@�dZ@�;d@�33@�+@���@���@��+@�n�@�E�@�J@��#@��-@��h@�/@��`@�j@�I�@�1@��F@��@���@�^5@���@���@��7@�p�@�7L@�bN@�(�@�1@��@���@���@�C�@��@�ff@��-@�O�@�%@��@��u@�bN@�9X@�b@���@���@��P@���@���@��+@�ff@���@�@��@��`@���@��@�z�@��@�I�@�1'@�ϫ@z�@hPH1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�bNA�ffA�jA�l�A�l�A�p�A�p�A�p�A�r�A�r�A�r�A�r�A�x�A�z�A�z�AуAсA�z�A�p�A�|�AуAщ7Aщ7AхAуAуAхAч+Aч+AыDAэPAэPAыDAэPAыDAэPAэPAя\AѓuAѓuAѕ�Aѕ�Aї�Aљ�Aљ�Aћ�Aћ�Aћ�Aћ�Aѝ�Aћ�Aѕ�AэPAэPAёhA�t�A�^5A�E�A�{A��;Aк^A�`BA�$�A�oA�A���A���Aϛ�A�^5A��TAΧ�A��HAȟ�Aę�A�jA��uA��RA��+A��A��wA��wA���A�1'A��9A���A��7A�S�A�$�A���A��yA�|�A�dZA��A�\)A�S�A���A��/A�(�A�ȴA�{A���A�+A��A��A�A�C�A�x�A�
=A�I�A�Q�A�JA�^5A���A�jA�;dA�ĜA��A�p�A�n�A�A���Ax(�At��Aq`BAo�FAm�-Ah�A`(�A]�AZz�AU�AQ7LAO"�AK�wAH1'AF�AC�-A@�uA>r�A=�
A<�A;�hA;�A:�yA:jA8�A7%A6A4~�A3\)A21A0v�A/��A/hsA.�A-�A+"�A*$�A)A(Q�A'�wA&�A%�-A$E�A#��A#��A#%A!&�A��AAA�A�hAM�A�uA�hA�jAbA/A�FA��A��Ar�A+A�A�FA�AffA��A$�A�TA��A+A��A��AffA1A�TA��A
ZA	t�A~�A$�AA�A�A�;A"�A�HAJA�/A�Al�A��AE�AffA r�@�^5@�O�@�Z@� �@ߥ�@�"�@��@ܬ@��@�(�@؃@�(�@�\)@�5?@ԓu@���@�@�~�@��#@�p�@��@��@��@��H@�\)@��y@�^5@ͩ�@�/@̛�@��;@˕�@�dZ@�"�@ʟ�@�J@ɩ�@ɑh@�O�@�/@���@�Z@�9X@�9X@�1'@ǅ@�v�@��@�J@�x�@��/@�r�@��
@�K�@�^5@�7L@��9@���@� �@��w@�33@���@�E�@�J@�x�@��9@��@�ȴ@�ȴ@��@�=q@��7@��@�V@���@���@��D@�(�@�S�@��#@��T@�hs@���@���@���@�%@��D@��@�l�@�@�5?@��T@�@�/@�V@���@��9@�9X@� �@�b@��@�t�@���@�K�@�t�@�;d@��H@�{@�@���@���@��h@��h@�x�@�X@���@��@���@���@�+@��\@��#@�p�@���@�I�@�1@��@��m@��@��P@�dZ@�"�@�o@�@��@��+@�J@��7@�X@�&�@��/@�j@�  @��;@�dZ@�
=@���@���@�{@�@��@�X@��@���@��@���@��@�"�@�C�@�dZ@���@�|�@�l�@�t�@�t�@�C�@�@��@��@���@��+@�$�@�x�@��@��D@�bN@�1'@�b@���@���@�C�@��H@���@�v�@�M�@�{@���@��7@�hs@�X@�7L@���@� �@���@��@�dZ@�;d@�33@�+@���@���@��+@�n�@�E�@�J@��#@��-@��h@�/@��`@�j@�I�@�1@��F@��@���@�^5@���@���@��7@�p�@�7L@�bN@�(�@�1@��@���@���@�C�@��@�ff@��-@�O�@�%@��@��u@�bN@�9X@�b@���@���@��P@���@���@��+@�ff@���@�@��@��`@���@��@�z�@��@�I�@�1'@�ϫ@z�@hPH1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBgmBgmBgmBgmBgmBgmBgmBgmBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBgmBgmBffBffBffBffBffBffBgmBhsBhsBgmBk�Bl�Bo�Bq�Br�Br�Bs�Br�Bq�Bp�Bo�Bn�Bm�BjBv�B�+B�{B�NBPB�#B�Bn�Bx�B�JB�VB�PB�DB�7B�1Bu�BffBe`BjBbNBM�B%�B�B&�B
=B��B�B�/BÖB�}B�FB��B�hB�Bu�BcTBM�B8RB#�BVB
��B
�B
�B
�TB
�
B
B
��B
n�B
N�B
-B
\B	��B	�B	e`B	K�B	?}B	0!B	oB�BŢB�LB��B��B��B��B�bB�%B�B~�B~�B~�B� B�B�B�B�B�+B�PB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�LB�LB�RB�qB�}B�}B�}B�wB�qB�dBĜB��B��B��B��B�
B�B�B��B��B�B�/B�)B�NB�B��B��B��B��B�B��B��B��B��B	B	+B	%B	B	oB	�B	�B	�B	#�B	$�B	'�B	&�B	%�B	%�B	$�B	"�B	!�B	#�B	+B	+B	)�B	-B	33B	49B	6FB	8RB	9XB	:^B	<jB	?}B	B�B	C�B	D�B	D�B	E�B	H�B	H�B	H�B	H�B	J�B	M�B	O�B	O�B	Q�B	S�B	T�B	W
B	YB	ZB	ZB	\)B	`BB	aHB	aHB	cTB	cTB	bNB	aHB	_;B	^5B	\)B	ZB	]/B	gmB	m�B	k�B	k�B	jB	jB	k�B	l�B	n�B	q�B	q�B	t�B	u�B	x�B	{�B	~�B	� B	� B	�B	�B	�B	�B	�%B	�%B	�7B	�JB	�PB	�VB	�\B	�bB	�hB	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�?B	�FB	�LB	�LB	�LB	�LB	�RB	�XB	�dB	�jB	�jB	�qB	�wB	�}B	��B	��B	ÖB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
1B
	7B

=B
JB
PB
JB
PB
VB
bB
hB
hB
�B
%�B
.�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 BffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBgmBgmBgmBgmBgmBgmBgmBgmBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBgmBgmBffBffBffBffBffBffBgmBhsBhsBgmBk�Bl�Bo�Bq�Br�Br�Bs�Br�Bq�Bp�Bo�Bn�Bm�BjBv�B�+B�{B�NBPB�#B�Bn�Bx�B�JB�VB�PB�DB�7B�1Bu�BffBe`BjBbNBM�B%�B�B&�B
=B��B�B�/BÖB�}B�FB��B�hB�Bu�BcTBM�B8RB#�BVB
��B
�B
�B
�TB
�
B
B
��B
n�B
N�B
-B
\B	��B	�B	e`B	K�B	?}B	0!B	oB�BŢB�LB��B��B��B��B�bB�%B�B~�B~�B~�B� B�B�B�B�B�+B�PB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�LB�LB�RB�qB�}B�}B�}B�wB�qB�dBĜB��B��B��B��B�
B�B�B��B��B�B�/B�)B�NB�B��B��B��B��B�B��B��B��B��B	B	+B	%B	B	oB	�B	�B	�B	#�B	$�B	'�B	&�B	%�B	%�B	$�B	"�B	!�B	#�B	+B	+B	)�B	-B	33B	49B	6FB	8RB	9XB	:^B	<jB	?}B	B�B	C�B	D�B	D�B	E�B	H�B	H�B	H�B	H�B	J�B	M�B	O�B	O�B	Q�B	S�B	T�B	W
B	YB	ZB	ZB	\)B	`BB	aHB	aHB	cTB	cTB	bNB	aHB	_;B	^5B	\)B	ZB	]/B	gmB	m�B	k�B	k�B	jB	jB	k�B	l�B	n�B	q�B	q�B	t�B	u�B	x�B	{�B	~�B	� B	� B	�B	�B	�B	�B	�%B	�%B	�7B	�JB	�PB	�VB	�\B	�bB	�hB	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�?B	�FB	�LB	�LB	�LB	�LB	�RB	�XB	�dB	�jB	�jB	�qB	�wB	�}B	��B	��B	ÖB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
1B
	7B

=B
JB
PB
JB
PB
VB
bB
hB
hB
�B
%�B
.�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190537                              AO  ARCAADJP                                                                    20181005190537    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190537  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190537  QCF$                G�O�G�O�G�O�8000            