CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:44Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191744  20181005191744  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��%F<1   @��%��ߦ@4��O�;d�d�bM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A!��A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C_�fCa�fCd  Cf  Ch  Cj  Ck�fCn�Cp�Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C��C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C��C��C��C��C�  C��3C��3C��3C��C�  C��3C�  C��3C�  C��C�  C��3C��3C��C��C�  C��3C��fC�  C��C��C�  C�  C�  C�  C��C��C��D fD � DfD� D��Dy�D��D� DfD�fD  Dy�D�3Dy�D��D� D  D�fD	�D	��D
fD
�fD  Dy�DfD�fD  Dy�D��Dy�D  D�fDfDy�D��Dy�D  Dy�D��D� D  Dy�DfD� D  Dy�D��D� D  Dy�D��D�fD  D� D  D� D��D�fD��D� D  D� D  Dy�D   D �fD �3D!y�D!��D"� D#  D#�fD$  D$�fD$�3D%y�D%��D&�fD'fD'y�D(fD(�fD)fD)y�D)��D*� D+  D+�fD,fD,y�D-  D-�fD.  D.�fD/fD/y�D0fD0�fD1  D1� D2fD2� D3fD3�fD4fD4y�D4��D5�fD6fD6�fD7fD7� D8fD8�fD9  D9y�D:  D:�fD;  D;y�D;��D<y�D<�3D=�fD>  D>� D?  D?y�D@  D@y�DA  DA�fDB  DBy�DB�3DCy�DC��DD� DEfDE�fDFfDF�fDG  DGy�DG��DHy�DI  DI� DJ  DJ�fDKfDK�fDLfDL�fDL��DM�fDN  DN� DN��DOy�Dy�
D�ED��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���A ��A"fgA@��A`��A�ffA�ffA�ffA���A�ffA�ffA�ffA�ffB ��B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B�L�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��gB��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(&gC*�C,�C.�C0�C2�C4�C6�C8�C:&gC<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\&gC^�C_�3Ca�3Cd�Cf�Ch�Cj�Ck�3Cn&gCp&gCr�Ct�Cv�Cx�Cz�C|&gC~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC���C���C�fC�fC�fC�fC�fC�3C�3C�fC���C���C���C���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�3C�fC�fC�fC�fC�3C�3C�3C�3C�fC�fC�fC�fC�fC�fC�3C�fC���C�fC�fC���C�3C�fC�fC�3C�3C�fC�fC�fC�fC�fC�fC�fC�fC���C�fC���C�fC�3C�3C�3C�3C�fC���C���C���C�3C�fC���C�fC���C�fC�3C�fC���C���C�3C�3C�fC���C���C�fC�3C�3C�fC�fC�fC�fC�3C�3C�3D 	�D �3D	�D�3D��D|�D��D�3D	�D��D3D|�D�fD|�D��D�3D3D��D	 D	� D
	�D
��D3D|�D	�D��D3D|�D��D|�D3D��D	�D|�D��D|�D3D|�D��D�3D3D|�D	�D�3D3D|�D��D�3D3D|�D��D��D3D�3D3D�3D��D��D��D�3D3D�3D3D|�D 3D ��D �fD!|�D!��D"�3D#3D#��D$3D$��D$�fD%|�D%��D&��D'	�D'|�D(	�D(��D)	�D)|�D)��D*�3D+3D+��D,	�D,|�D-3D-��D.3D.��D/	�D/|�D0	�D0��D13D1�3D2	�D2�3D3	�D3��D4	�D4|�D4��D5��D6	�D6��D7	�D7�3D8	�D8��D93D9|�D:3D:��D;3D;|�D;��D<|�D<�fD=��D>3D>�3D?3D?|�D@3D@|�DA3DA��DB3DB|�DB�fDC|�DC��DD�3DE	�DE��DF	�DF��DG3DG|�DG��DH|�DI3DI�3DJ3DJ��DK	�DK��DL	�DL��DL��DM��DN3DN�3DN��DO|�Dy�=D�F�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aơ�Aơ�Aƥ�Aƥ�Aƥ�Aƥ�Aƥ�Aƥ�Aƥ�Aơ�AƓuA�bNAōPA���A��TA���Aĥ�AđhAăA�|�A�v�A�p�A�ffA�`BA�O�A�G�A�A�A�/A�$�A��A��A�bA��AÓuA�l�A�=qA�A��;A�A�"�A�K�A��RA�9XA���A��uA���A�G�A���A���A�v�A���A�K�A�M�A��+A�Q�A�A���A��A�l�A��;A�%A��A�;dA��9A��jA�oA�~�A�ffA��A��7A�|�A���A��;A���A��A�ffA���A��A��^A���A�p�A���A��A�XA�G�A�z�A�l�A�-A���A�bA�l�A���A��7A���A��wA��A��uA�VA�K�A�^5A���A���A��A�-A�&�A��A���A�1'A�1'A}x�AxjAqx�Am��AjJAhJAgK�Af�\Ad�DA`ZA]�A[C�AY?}AX��AW�PAV��AU?}ATE�AQ�wAO��AOx�AO7LAN9XAM��AL�jALbAJ�yAI�;AIAH�+AH9XAF��AD�HAC`BAA��A@{A?`BA>�RA>M�A=�^A;+A9�#A8��A8$�A7|�A6�/A5��A3�A3x�A2ZA1�^A1|�A/�^A.z�A,(�A)��A'|�A&~�A$5?A"�`A �`A��A�mA�A`BA�TAdZA9XAO�A�jA��A�DAbA`BAbAƨA\)A
=An�A��A��A
=A�TA�A��An�A^5A=qA�A
bNA	C�AJA33A��A��A�yAS�A9XAoA �u@��!@���@���@�v�@��@�5?@�/@�j@�\)@�O�@��H@��/@��
@�@�-@��@�j@�h@�1@��@�?}@�j@߾w@�E�@�x�@ە�@��H@�M�@���@��@�(�@�C�@��@�n�@�G�@�I�@���@Ӿw@��y@���@���@��@�$�@��@�~�@ȃ@ư!@�X@��;@�C�@��H@�E�@���@��@��@��u@�r�@�I�@� �@�ƨ@�C�@���@�~�@�^5@��T@�hs@���@��@�x�@��@�Ĝ@���@���@��u@�l�@���@�hs@�X@�/@�Ĝ@��9@�Z@���@��@�\)@�o@�@���@���@��R@���@��+@�^5@�ff@�-@�J@�5?@�E�@�M�@�n�@�~�@�~�@�v�@�v�@�M�@�@���@�G�@�V@�9X@��w@��w@���@�
=@��+@�-@��#@�%@�j@���@��@�-@�?}@���@��@��m@���@�5?@��@��^@�X@�7L@�7L@�/@�V@�%@��`@��j@�Q�@�(�@��@��F@�C�@�+@�"�@��@�ff@�J@���@�G�@�S�@�n�@�E�@���@�7L@��@��@���@���@��9@���@�r�@�A�@�9X@�9X@�(�@�b@���@���@��m@���@��F@�\)@�o@��@���@�M�@��@��@�v�@��7@�?}@�7L@���@�bN@�9X@�r�@�(�@���@��@�  @�ƨ@��w@�+@��y@���@�~�@�M�@���@��#@���@��-@�hs@�7L@��@��@r{@bȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aơ�Aơ�Aƥ�Aƥ�Aƥ�Aƥ�Aƥ�Aƥ�Aƥ�Aơ�AƓuA�bNAōPA���A��TA���Aĥ�AđhAăA�|�A�v�A�p�A�ffA�`BA�O�A�G�A�A�A�/A�$�A��A��A�bA��AÓuA�l�A�=qA�A��;A�A�"�A�K�A��RA�9XA���A��uA���A�G�A���A���A�v�A���A�K�A�M�A��+A�Q�A�A���A��A�l�A��;A�%A��A�;dA��9A��jA�oA�~�A�ffA��A��7A�|�A���A��;A���A��A�ffA���A��A��^A���A�p�A���A��A�XA�G�A�z�A�l�A�-A���A�bA�l�A���A��7A���A��wA��A��uA�VA�K�A�^5A���A���A��A�-A�&�A��A���A�1'A�1'A}x�AxjAqx�Am��AjJAhJAgK�Af�\Ad�DA`ZA]�A[C�AY?}AX��AW�PAV��AU?}ATE�AQ�wAO��AOx�AO7LAN9XAM��AL�jALbAJ�yAI�;AIAH�+AH9XAF��AD�HAC`BAA��A@{A?`BA>�RA>M�A=�^A;+A9�#A8��A8$�A7|�A6�/A5��A3�A3x�A2ZA1�^A1|�A/�^A.z�A,(�A)��A'|�A&~�A$5?A"�`A �`A��A�mA�A`BA�TAdZA9XAO�A�jA��A�DAbA`BAbAƨA\)A
=An�A��A��A
=A�TA�A��An�A^5A=qA�A
bNA	C�AJA33A��A��A�yAS�A9XAoA �u@��!@���@���@�v�@��@�5?@�/@�j@�\)@�O�@��H@��/@��
@�@�-@��@�j@�h@�1@��@�?}@�j@߾w@�E�@�x�@ە�@��H@�M�@���@��@�(�@�C�@��@�n�@�G�@�I�@���@Ӿw@��y@���@���@��@�$�@��@�~�@ȃ@ư!@�X@��;@�C�@��H@�E�@���@��@��@��u@�r�@�I�@� �@�ƨ@�C�@���@�~�@�^5@��T@�hs@���@��@�x�@��@�Ĝ@���@���@��u@�l�@���@�hs@�X@�/@�Ĝ@��9@�Z@���@��@�\)@�o@�@���@���@��R@���@��+@�^5@�ff@�-@�J@�5?@�E�@�M�@�n�@�~�@�~�@�v�@�v�@�M�@�@���@�G�@�V@�9X@��w@��w@���@�
=@��+@�-@��#@�%@�j@���@��@�-@�?}@���@��@��m@���@�5?@��@��^@�X@�7L@�7L@�/@�V@�%@��`@��j@�Q�@�(�@��@��F@�C�@�+@�"�@��@�ff@�J@���@�G�@�S�@�n�@�E�@���@�7L@��@��@���@���@��9@���@�r�@�A�@�9X@�9X@�(�@�b@���@���@��m@���@��F@�\)@�o@��@���@�M�@��@��@�v�@��7@�?}@�7L@���@�bN@�9X@�r�@�(�@���@��@�  @�ƨ@��w@�+@��y@���@�~�@�M�@���@��#@���@��-@�hs@�7L@��@��@r{@bȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�'B�'B�-B�3B�3B�3B�9B�9B�9B�LB�}B��BB#�B,B49B@�BE�BI�BK�BL�BM�BO�BP�BT�BXB[#BbNBcTBcTBaHBdZBgmBv�B}�B�%B�oB��B��B�dB�B�yB�)B��B��BŢBÖB��B�qB�^B�9B�B��B��B�-B�LB��B�^B�LB�?B��B��B��B��B�\B�+B{�Bu�Bp�BjBYBO�BE�BE�BF�BG�B@�B6FB,B �BoB1B��B�sB�)B��B��B��BƨB�}B�FB�B��B�{B�JB�Bo�BVB:^B�B
�NB
��B
�-B
��B
��B
~�B
n�B
`BB
M�B
49B
JB	��B	�wB	�-B	��B	��B	��B	�bB	{�B	jB	{�B	�B	�B	z�B	r�B	hsB	bNB	XB	N�B	L�B	J�B	E�B	A�B	<jB	7LB	1'B	+B	&�B	#�B	!�B	�B	uB	JB	%B��B��B��B�B�B�fB�NB�;B�/B�/B�#B�B��B��B��B��B��BƨB�}B�RB�!B�B�B��B��B��B��B��B��B�{B�\B�\B�JB�=B�7B�7B�1B�+B�B�B�B�B�B�B�B� B� B�B�B�B�B�B� B~�B~�B� B�B�B� B�B� B� B~�B|�B{�By�Bx�Bw�Bw�Bv�Bw�Bw�Bx�Bw�Bw�Bz�B}�B}�B|�B}�B~�B� B�1B�DB�PB�\B�bB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�\B�PB�DB�PB�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�B�B�B�B�!B�3B�9B�9B�?B�LB�^B��B��BÖBŢBȴB��B��B��B��B�B�)B�BB�ZB�B��B��B��B	  B	B	B	B	B	B	%B	
=B	VB	oB	�B	�B	!�B	!�B	!�B	#�B	%�B	&�B	'�B	-B	0!B	49B	8RB	?}B	D�B	I�B	M�B	N�B	T�B	XB	YB	[#B	]/B	_;B	`BB	`BB	bNB	cTB	dZB	e`B	iyB	jB	jB	l�B	q�B	q�B	q�B	s�B	v�B	y�B	{�B	{�B	w�B	s�B	t�B	u�B	z�B	}�B	~�B	� B	� B	�B	�B	�B	�B	�%B	�JB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�B
�B
*�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�'B�'B�-B�3B�3B�3B�9B�9B�9B�LB�}B��BB#�B,B49B@�BE�BI�BK�BL�BM�BO�BP�BT�BXB[#BbNBcTBcTBaHBdZBgmBv�B}�B�%B�oB��B��B�dB�B�yB�)B��B��BŢBÖB��B�qB�^B�9B�B��B��B�-B�LB��B�^B�LB�?B��B��B��B��B�\B�+B{�Bu�Bp�BjBYBO�BE�BE�BF�BG�B@�B6FB,B �BoB1B��B�sB�)B��B��B��BƨB�}B�FB�B��B�{B�JB�Bo�BVB:^B�B
�NB
��B
�-B
��B
��B
~�B
n�B
`BB
M�B
49B
JB	��B	�wB	�-B	��B	��B	��B	�bB	{�B	jB	{�B	�B	�B	z�B	r�B	hsB	bNB	XB	N�B	L�B	J�B	E�B	A�B	<jB	7LB	1'B	+B	&�B	#�B	!�B	�B	uB	JB	%B��B��B��B�B�B�fB�NB�;B�/B�/B�#B�B��B��B��B��B��BƨB�}B�RB�!B�B�B��B��B��B��B��B��B�{B�\B�\B�JB�=B�7B�7B�1B�+B�B�B�B�B�B�B�B� B� B�B�B�B�B�B� B~�B~�B� B�B�B� B�B� B� B~�B|�B{�By�Bx�Bw�Bw�Bv�Bw�Bw�Bx�Bw�Bw�Bz�B}�B}�B|�B}�B~�B� B�1B�DB�PB�\B�bB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�\B�PB�DB�PB�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�B�B�B�B�!B�3B�9B�9B�?B�LB�^B��B��BÖBŢBȴB��B��B��B��B�B�)B�BB�ZB�B��B��B��B	  B	B	B	B	B	B	%B	
=B	VB	oB	�B	�B	!�B	!�B	!�B	#�B	%�B	&�B	'�B	-B	0!B	49B	8RB	?}B	D�B	I�B	M�B	N�B	T�B	XB	YB	[#B	]/B	_;B	`BB	`BB	bNB	cTB	dZB	e`B	iyB	jB	jB	l�B	q�B	q�B	q�B	s�B	v�B	y�B	{�B	{�B	w�B	s�B	t�B	u�B	z�B	}�B	~�B	� B	� B	�B	�B	�B	�B	�%B	�JB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�B
�B
*�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191744                              AO  ARCAADJP                                                                    20181005191744    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191744  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191744  QCF$                G�O�G�O�G�O�8000            