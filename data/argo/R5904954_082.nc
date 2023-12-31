CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:07Z creation      
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
_FillValue                 �  @L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Qt   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  XP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  b�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20181005191707  20181005191707  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               RA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @����*1   @���#`@4�
=p���d/��$�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      RA   A   B   @�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C-�fC0  C2  C4�C6�C8  C9�fC<  C>  C?�fCA�fCD  CF  CH  CI�fCL  CN  CP  CR�CT�CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Cg�fCi�fCl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~�C��C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��C��C��C��C��C�  C�  C��3C�  C��C��C�  C�  C��C��C�  C��C�  C�  C��3C�  C��C��3C�  C�  C�  C��C��3C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C��C��3C��3C��3C�  C�  C�  C�  C��C�  C��3C�  C��fC��3C��3C�  C�  C��3C�  C��3C�  C��3C��C�  C��C��C�  C��3C��fC��3C��C�  C�  C��C��C��C��C��C��C��3C�  C��C��C��C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C��3C��3C�  C�  C��3C�  C��3C�  C��C��C�  C��fC��3C�  D fD � D  D�fD  Dy�D  D� D  D� DfD� D��D�fD  D� D  Dy�D	  D	�fD	��D
y�DfD� D��D� D  D� D��Dy�D  D� D  D�fD  Dy�D��Dy�D  D�fDfD�fD��Ds3D��D�fD��Dy�DfD� D  D� D  D�fDfD�fD  Dy�D��D� D  Dy�D  D�fD   D �fD!  D!� D"  D"�fD#fD#� D$fD$�fD$��D%� D&fD&�fD'fD'� D'��D(y�D)  D)y�D)��D*� D+fD+�fD,fD,�fD-  D-y�D.  D.� D.��D/y�D0fD0�fD1  D1y�D2  D2y�D3  D3y�D3��D4�fD5fD5� D6fD6�fD7fD7y�D8fD8y�D9fD9�fD9��D:y�D;  D;� D;��D<y�D<��D=y�D=�3D>� D?fD?�fD@fD@�fDA  DA� DA��DB�fDB��DC� DDfDD� DPy�DQ  DQ� DQ��DR�fDR��DS�fDS��DT�fDU  DU� DV�DV� DWfDWy�DW��DX�fDY  DY�fDZ�DZ�fDZ�3D[y�D\fD\�fD\��D]y�D^  D^� D_fD_�fD`fD`� D`��Day�Da��Db� Dc  Dcy�Dc��Dd� De  De� Df  Dy� D�B�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@�33A��A#34AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&  C(�C*�C,�C.  C0�C2�C434C634C8�C:  C<�C>�C@  CB  CD�CF�CH�CJ  CL�CN�CP�CR34CT34CV�CX�CZ�C\�C^�C`  Cb�Cd�Cf�Ch  Cj  Cl�Cn�Cp�Cr�Ct�Cv�Cx34Cz�C|�C~34C��C�  C�  C�  C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C�  C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C��C��C��C��C��C��C�  C��C��3C�  C�  C��C��C�  C��C�  C��C�  C��C��C��C��C��C�  C��3C�  C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C�  C�  C�  C��C��C�  C��C�  C��C��C�&gC��C��3C�  C��D �D �fDfD��DfD� DfD�fDfD�fD�D�fD  D��DfD�fDfD� D	fD	��D
  D
� D�D�fD  D�fDfD�fD  D� DfD�fDfD��DfD� D  D� DfD��D�D��D  Dy�D  D��D  D� D�D�fDfD�fDfD��D�D��DfD� D  D�fDfD� DfD��D fD ��D!fD!�fD"fD"��D#�D#�fD$�D$��D%  D%�fD&�D&��D'�D'�fD(  D(� D)fD)� D*  D*�fD+�D+��D,�D,��D-fD-� D.fD.�fD/  D/� D0�D0��D1fD1� D2fD2� D3fD3� D4  D4��D5�D5�fD6�D6��D7�D7� D8�D8� D9�D9��D:  D:� D;fD;�fD<  D<� D=  D=� D=��D>�fD?�D?��D@�D@��DAfDA�fDB  DB��DC  DC�fDD�DD�fDP� DQfDQ�fDR  DR��DS  DS��DT  DT��DUfDU�fDV3DV�fDW�DW� DX  DX��DYfDY��DZ3DZ��DZ��D[� D\�D\��D]  D]� D^fD^�fD_�D_��D`�D`�fDa  Da� Db  Db�fDcfDc� Dd  Dd�fDefDe�fDffDy�fD�FD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��`A���A���A���A���A���A���A�  A���A�  A�A���A��A��;AۍPAڰ!A�+A�E�A�
=A�9XA͙�A�G�Aˣ�Aˉ7A�  A�/A�z�A��#AŁA�|�AÓuA¬A�|�A���A��TA��PA���A�{A���A���A�`BA�^5A��FA���A��A�E�A���A��wA�  A�$�A��
A��7A�=qA��DA�/A�ĜA�Q�A���A��/A���A�ZA��!A�A�A�A�ZA��A�n�A��A�^5A�t�A���A��jA�+A�bNA�%A��+A��PA�A��9A��A���A�K�A��\A�+A�jA�jA�bNA�`BA�$�A�+A�E�A��yA�"�A��A�(�A�ƨA��hA��A��FA���A���A��A|bAy��Axz�AvM�Ar(�Am��AjQ�Ag�Ad�A`�A_33A^��AZ^5AW&�AV1'AT�ARr�AQ�hAO?}AM�FAM�AL��AL�AK|�AJĜAJ=qAI
=AF�HAE�AE7LAD�!ACS�AA�
A?C�A=XA<��A<��A<�\A<v�A;x�A9�A7��A6M�A4�DA2�A1?}A01'A.=qA,VA+G�A)��A'�hA%��A$z�A"jA!oA (�A�A��AQ�A1'AA�/A�A �AƨA��At�A�/A�uAZA  A�AQ�Al�A�A�A�
A7LAQ�A��Al�A�A1'AƨAx�A��A�A+A
n�A�Ax�AĜAK�A~�A9XA+A �9@���@�`B@���@��@���@��u@�x�@���@�"�@��@�+@�{@��^@��@�  @�(�@�\)@��@��@�p�@�9@��@�7L@�V@�1'@�33@�-@���@�ƨ@�@�%@���@ա�@ԣ�@�C�@ҸR@��`@�l�@�@��/@�(�@˾w@��@�&�@Ǿw@�S�@�-@��@�|�@���@��9@���@���@��D@��D@���@�bN@��@��j@���@��+@�@��@��@�1'@���@�t�@�ff@�J@�x�@�(�@��F@��@���@�V@�V@���@���@�dZ@�
=@���@��j@��9@��@�J@�`B@�`B@��@��7@��9@��@��;@���@���@��@�M�@�dZ@���@��@�hs@���@���@�|�@�S�@�V@�O�@��D@���@�O�@���@��w@��!@���@��R@�;d@��m@���@� �@�I�@��D@�9X@��@�t�@�1@�hs@��@�r�@���@�I�@�A�@��@��;@��;@�I�@��@�\)@�33@��+@���@�`B@��@��@�A�@�1'@��u@��/@��@�Ĝ@�Z@�9X@��m@�t�@�C�@��@���@�M�@�-@��-@��`@��@��u@�z�@�I�@���@���@�K�@��@���@���@�^5@�-@��@��@��`@��@��D@��D@�bN@�1'@��F@�l�@�;d@�+@���@���@��!@���@��\@�~�@�ff@��-@�z�@���@��@�j@�b@��
@��;@�1@��@� �@��w@�|�@�C�@��@��@���@���@�n�@�=q@�-@�5?@�{@���@��#@���@���@�b@���@�dZ@�
=@��y@���@��\@�E�@���@�G�@��`@�j@�(�@��@�b@�1@��@���@��@��b@q+�@_�r1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��`A���A���A���A���A���A���A�  A���A�  A�A���A��A��;AۍPAڰ!A�+A�E�A�
=A�9XA͙�A�G�Aˣ�Aˉ7A�  A�/A�z�A��#AŁA�|�AÓuA¬A�|�A���A��TA��PA���A�{A���A���A�`BA�^5A��FA���A��A�E�A���A��wA�  A�$�A��
A��7A�=qA��DA�/A�ĜA�Q�A���A��/A���A�ZA��!A�A�A�A�ZA��A�n�A��A�^5A�t�A���A��jA�+A�bNA�%A��+A��PA�A��9A��A���A�K�A��\A�+A�jA�jA�bNA�`BA�$�A�+A�E�A��yA�"�A��A�(�A�ƨA��hA��A��FA���A���A��A|bAy��Axz�AvM�Ar(�Am��AjQ�Ag�Ad�A`�A_33A^��AZ^5AW&�AV1'AT�ARr�AQ�hAO?}AM�FAM�AL��AL�AK|�AJĜAJ=qAI
=AF�HAE�AE7LAD�!ACS�AA�
A?C�A=XA<��A<��A<�\A<v�A;x�A9�A7��A6M�A4�DA2�A1?}A01'A.=qA,VA+G�A)��A'�hA%��A$z�A"jA!oA (�A�A��AQ�A1'AA�/A�A �AƨA��At�A�/A�uAZA  A�AQ�Al�A�A�A�
A7LAQ�A��Al�A�A1'AƨAx�A��A�A+A
n�A�Ax�AĜAK�A~�A9XA+A �9@���@�`B@���@��@���@��u@�x�@���@�"�@��@�+@�{@��^@��@�  @�(�@�\)@��@��@�p�@�9@��@�7L@�V@�1'@�33@�-@���@�ƨ@�@�%@���@ա�@ԣ�@�C�@ҸR@��`@�l�@�@��/@�(�@˾w@��@�&�@Ǿw@�S�@�-@��@�|�@���@��9@���@���@��D@��D@���@�bN@��@��j@���@��+@�@��@��@�1'@���@�t�@�ff@�J@�x�@�(�@��F@��@���@�V@�V@���@���@�dZ@�
=@���@��j@��9@��@�J@�`B@�`B@��@��7@��9@��@��;@���@���@��@�M�@�dZ@���@��@�hs@���@���@�|�@�S�@�V@�O�@��D@���@�O�@���@��w@��!@���@��R@�;d@��m@���@� �@�I�@��D@�9X@��@�t�@�1@�hs@��@�r�@���@�I�@�A�@��@��;@��;@�I�@��@�\)@�33@��+@���@�`B@��@��@�A�@�1'@��u@��/@��@�Ĝ@�Z@�9X@��m@�t�@�C�@��@���@�M�@�-@��-@��`@��@��u@�z�@�I�@���@���@�K�@��@���@���@�^5@�-@��@��@��`@��@��D@��D@�bN@�1'@��F@�l�@�;d@�+@���@���@��!@���@��\@�~�@�ff@��-@�z�@���@��@�j@�b@��
@��;@�1@��@� �@��w@�|�@�C�@��@��@���@���@�n�@�=q@�-@�5?@�{@���@��#@���@���@�b@���@�dZ@�
=@��y@���@��\@�E�@���@�G�@��`@�j@�(�@��@�b@�1@��@���@��@��b@q+�@_�r1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B#�B"�B �B"�B"�B"�B"�B"�B"�B"�B"�B!�B �B�B�B\B��B�B�B��BB\BhB\BPBoB�B+B9XBC�BM�BR�B]/B_;BcTBk�Bp�Br�Bv�B|�B{�B}�B~�B�B~�Bz�Bv�Bp�Bt�Be`BcTBdZB}�B|�Bo�Bs�B�B�B�Bz�Bv�Bo�BffBbNB\)BZBK�B<jB6FB-B#�B�BVBB�B�B�'B��B�oB|�Bt�Bo�BffB]/BH�B7LB)�B�B
��B
��B
�HB
�;B
��B
��B
ĜB
�B
��B
�7B
|�B
o�B
aHB
C�B
&�B
�B

=B	��B	�)B	��B	��B	�uB	�B	m�B	cTB	\)B	J�B	<jB	5?B	0!B	"�B	�B	oB	DB		7B	%B	B	B��B��B�B�yB�fB�ZB�BB�#B��B��BɺBȴBǮBƨBĜB��B�qB�RB�9B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�\B�7B�B�1B�JB�7B�B~�B� B� B~�B~�B�B� B�B�B�%B}�B� By�Bx�B�%B�+B�+B�%B�B}�Bw�Bx�By�Bw�Bv�Bt�Bt�Bt�Bu�Bs�Bt�Br�Bq�Bq�Bq�Bp�Bp�Bp�Bo�Bo�Bo�Bn�Bq�Bt�Bu�Bx�B�B�B�B�VB�JB�=B�+B�B�B�B�B�+B�7B�=B�JB�VB�\B�oB��B��B��B��B��B��B��B��B�B�B�B�-B�FB�RB�qB��BƨBɺB��B��B��B�
B�5B�ZB�B��B	+B	DB	bB	�B	�B	(�B	+B	(�B	+B	)�B	$�B	�B	�B	�B	�B	�B	#�B	)�B	1'B	49B	6FB	9XB	=qB	@�B	A�B	A�B	H�B	Q�B	T�B	P�B	R�B	S�B	T�B	W
B	YB	ZB	_;B	e`B	ffB	gmB	jB	m�B	m�B	o�B	r�B	u�B	w�B	�B	�%B	�=B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�LB	�XB	�^B	�qB	�qB	��B	B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
1B
1B

=B

=B
2B
!�B
,2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222 B#�B"�B �B"�B"�B"�B"�B"�B"�B"�B"�B!�B �B�B�B\B��B�B�B��BB\BhB\BPBoB�B+B9XBC�BM�BR�B]/B_;BcTBk�Bp�Br�Bv�B|�B{�B}�B~�B�B~�Bz�Bv�Bp�Bt�Be`BcTBdZB}�B|�Bo�Bs�B�B�B�Bz�Bv�Bo�BffBbNB\)BZBK�B<jB6FB-B#�B�BVBB�B�B�'B��B�oB|�Bt�Bo�BffB]/BH�B7LB)�B�B
��B
��B
�HB
�;B
��B
��B
ĜB
�B
��B
�7B
|�B
o�B
aHB
C�B
&�B
�B

=B	��B	�)B	��B	��B	�uB	�B	m�B	cTB	\)B	J�B	<jB	5?B	0!B	"�B	�B	oB	DB		7B	%B	B	B��B��B�B�yB�fB�ZB�BB�#B��B��BɺBȴBǮBƨBĜB��B�qB�RB�9B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�\B�7B�B�1B�JB�7B�B~�B� B� B~�B~�B�B� B�B�B�%B}�B� By�Bx�B�%B�+B�+B�%B�B}�Bw�Bx�By�Bw�Bv�Bt�Bt�Bt�Bu�Bs�Bt�Br�Bq�Bq�Bq�Bp�Bp�Bp�Bo�Bo�Bo�Bn�Bq�Bt�Bu�Bx�B�B�B�B�VB�JB�=B�+B�B�B�B�B�+B�7B�=B�JB�VB�\B�oB��B��B��B��B��B��B��B��B�B�B�B�-B�FB�RB�qB��BƨBɺB��B��B��B�
B�5B�ZB�B��B	+B	DB	bB	�B	�B	(�B	+B	(�B	+B	)�B	$�B	�B	�B	�B	�B	�B	#�B	)�B	1'B	49B	6FB	9XB	=qB	@�B	A�B	A�B	H�B	Q�B	T�B	P�B	R�B	S�B	T�B	W
B	YB	ZB	_;B	e`B	ffB	gmB	jB	m�B	m�B	o�B	r�B	u�B	w�B	�B	�%B	�=B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�LB	�XB	�^B	�qB	�qB	��B	B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
1B
1B

=B

=B
2B
!�B
,2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.10 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191707                              AO  ARCAADJP                                                                    20181005191707    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191707  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005191707  QCF$                G�O�G�O�G�O�8000            