CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:54Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140854  20181024140854  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @����O�1   @���'ҏ�@5��O�;d�c�(�\1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�33@�  A   AffA@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.�C0�C2  C4  C6  C8  C:  C<�C>  C@  CA�fCC�fCF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C��3C��3C��3C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD	  D	� D
  D
� D  D� D  Dy�D  D� D  D� D��D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D*��D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG�fDH  DH�fDI  DI� DJ  DJ� DK  DK�fDLfDL� DM  DM� DN  DN� DO  DO� Dy�\D�L)D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@ʏ\AG�A#�AEG�AeG�A��
A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B(�B1Q�B9Q�BAQ�BI�RBQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�u�B�u�BĨ�BȨ�B̨�BШ�BԨ�B��)Bܨ�B��B��B��B��B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C":�C$T{C&T{C(T{C*T{C,T{C.nC0nC2T{C4T{C6T{C8T{C:T{C<nC>T{C@T{CB:�CD:�CFT{CHT{CJT{CLT{CN:�CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{Ch:�CjT{ClT{CnT{CpT{Cr:�CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�pC�pC�pC�pC�*=C�*=C�*=C�pC�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=D D �DD�DD�DD�DD�DD�DD�DD�D�D��D	D	�D
D
�DD�DD��DD�DD�D�D�DD�DD�D�D�DD�DD�DD�DD�DD�DD�D�D�DD�DD�DD�DD�DD�D�D�D D �D!D!��D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+�D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1��D2D2�D3D3�D4D4�D5D5�D6D6�D7�D7��D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DF�DF�DGDG��DHDH��DIDI�DJDJ�DKDK��DL�DL�DMDM�DNDN�DODO�Dy�{D�V�D�ٙ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��jA���A���A�ĜA�ĜA�ȴA���A���A���A�ȴA��wA��wA��wA���A���A���A���A���A���A���A���A���A�r�A��RA��`A�|�A�S�A�;dA�5?A��A���A��A��PA�jA�K�A��A��A�ĜA��RA���A�`BA�bA�~�A��A��A��A��A���A��+A�t�A�1'A�M�A��A��#A��DA�K�A�ZA�=qA�(�A��#A�O�A���A�7LA�VA�A�S�A��A�bA��9A�9XA��yA���A��A���A�x�A�Q�A�33A�bA��FA�&�A��PA���A�5?A��A��A�9XA��`A�ZA�ZA��A��hA���A�+A���A���A��A���A�VA���A�?}A���A���A�A�A��7A�oA�ZA��jA�C�A`BA}hsA{��Az��AzVAy��Ay
=Aup�AtbNAsdZAq�AnZAjbNAh1Ae�hAb�Aa�hA`�HA_��A^�HA]��A\=qA[��AZ��AY33AX �AW"�AU?}ATz�AS�AR1AQp�AO�
AM�AK`BAI�wAHr�AHAF��AAA?S�A>��A> �A=��A<�HA<9XA;��A;p�A;K�A;7LA:��A9XA7�A5C�A3��A2�HA133A/|�A.^5A,��A)�A&1'A$�yA#��A#t�A#`BA"1A�A�+A��AVAXA&�AoA�!AƨA|�A��AG�Al�A��A��A�AG�A��AVA�hA�A��A��A�#Al�A
�A	�7A	oAZA��AbNA�A�uA�-A+A��A ȴ@��F@�hs@��
@���@�+@���@�v�@���@��@�9X@��@�@�bN@��@�w@��@�5?@���@�@�(�@�F@�C�@�R@�n�@��T@�t�@�z�@�G�@�7L@��@�V@��@ؼj@׾w@�\)@��@ָR@�v�@�J@ա�@�G�@ԣ�@�bN@�bN@�bN@�bN@�(�@�|�@Ұ!@ҏ\@�n�@�~�@�~�@�v�@�M�@���@��;@�p�@��@ˍP@�;d@���@��@�p�@��m@�ff@���@�7L@��`@�z�@��m@�@�^5@�E�@��@���@���@�G�@��@�1'@��@�b@��w@�+@��@�I�@�=q@�hs@�/@�&�@��@��@�Ĝ@���@�ȴ@��+@�^5@�^5@�$�@��^@���@�
=@���@�@�/@���@���@��9@��D@�A�@�1@�dZ@�~�@��@�%@��@�Q�@�Z@�j@�z�@�9X@��@��@�ȴ@�V@��\@�5?@��@��H@��-@�/@���@��@���@�
=@�V@�hs@�{@��T@�hs@���@���@�bN@�Z@�I�@�A�@��@�l�@�C�@�+@��@�M�@�^5@�v�@�^5@�-@�{@�@���@��^@��-@��@�p�@�hs@�O�@�V@�j@�Q�@�(�@�(�@�1@��w@�\)@�o@��R@�M�@���@�hs@�O�@��7@�hs@���@�1'@��w@��@�t�@�K�@��!@��-@�X@��`@�j@�r�@���@��D@�I�@�I�@�(�@��@�\)@��@�o@~��@p��@bn�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��jA���A���A�ĜA�ĜA�ȴA���A���A���A�ȴA��wA��wA��wA���A���A���A���A���A���A���A���A���A�r�A��RA��`A�|�A�S�A�;dA�5?A��A���A��A��PA�jA�K�A��A��A�ĜA��RA���A�`BA�bA�~�A��A��A��A��A���A��+A�t�A�1'A�M�A��A��#A��DA�K�A�ZA�=qA�(�A��#A�O�A���A�7LA�VA�A�S�A��A�bA��9A�9XA��yA���A��A���A�x�A�Q�A�33A�bA��FA�&�A��PA���A�5?A��A��A�9XA��`A�ZA�ZA��A��hA���A�+A���A���A��A���A�VA���A�?}A���A���A�A�A��7A�oA�ZA��jA�C�A`BA}hsA{��Az��AzVAy��Ay
=Aup�AtbNAsdZAq�AnZAjbNAh1Ae�hAb�Aa�hA`�HA_��A^�HA]��A\=qA[��AZ��AY33AX �AW"�AU?}ATz�AS�AR1AQp�AO�
AM�AK`BAI�wAHr�AHAF��AAA?S�A>��A> �A=��A<�HA<9XA;��A;p�A;K�A;7LA:��A9XA7�A5C�A3��A2�HA133A/|�A.^5A,��A)�A&1'A$�yA#��A#t�A#`BA"1A�A�+A��AVAXA&�AoA�!AƨA|�A��AG�Al�A��A��A�AG�A��AVA�hA�A��A��A�#Al�A
�A	�7A	oAZA��AbNA�A�uA�-A+A��A ȴ@��F@�hs@��
@���@�+@���@�v�@���@��@�9X@��@�@�bN@��@�w@��@�5?@���@�@�(�@�F@�C�@�R@�n�@��T@�t�@�z�@�G�@�7L@��@�V@��@ؼj@׾w@�\)@��@ָR@�v�@�J@ա�@�G�@ԣ�@�bN@�bN@�bN@�bN@�(�@�|�@Ұ!@ҏ\@�n�@�~�@�~�@�v�@�M�@���@��;@�p�@��@ˍP@�;d@���@��@�p�@��m@�ff@���@�7L@��`@�z�@��m@�@�^5@�E�@��@���@���@�G�@��@�1'@��@�b@��w@�+@��@�I�@�=q@�hs@�/@�&�@��@��@�Ĝ@���@�ȴ@��+@�^5@�^5@�$�@��^@���@�
=@���@�@�/@���@���@��9@��D@�A�@�1@�dZ@�~�@��@�%@��@�Q�@�Z@�j@�z�@�9X@��@��@�ȴ@�V@��\@�5?@��@��H@��-@�/@���@��@���@�
=@�V@�hs@�{@��T@�hs@���@���@�bN@�Z@�I�@�A�@��@�l�@�C�@�+@��@�M�@�^5@�v�@�^5@�-@�{@�@���@��^@��-@��@�p�@�hs@�O�@�V@�j@�Q�@�(�@�(�@�1@��w@�\)@�o@��R@�M�@���@�hs@�O�@��7@�hs@���@�1'@��w@��@�t�@�K�@��!@��-@�X@��`@�j@�r�@���@��D@�I�@�I�@�(�@��@�\)@��@�o@~��@p��@bn�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BuBoBuBoBuBuBuBuBuBuBuBuBuBuBoBoBhBoBoBoBuB�B#�B1'B?}BH�BL�BO�BO�BQ�BS�BVB[#B^5B`BBbNBdZBgmBgmBcTBgmBffBiyBp�By�B~�B�B�B�B�B�B�B�B�B� B� B�7B�1B�+B�%B�By�Bn�BjBe`B_;BZBQ�BC�B=qB9XB6FB33B.B�BB��B�B�BB�B��BƨB�FB��B��B��B��B��B��B�hB�1B�B|�By�Bu�BjBD�B&�B�B  B
�sB
ȴB
�B
��B
��B
�{B
�JB
�B
{�B
o�B
cTB
^5B
ZB
VB
M�B
5?B
-B
#�B
�B
  B	�mB	�B	ȴB	�XB	�'B	�B	��B	��B	��B	�uB	�hB	�DB	�B	{�B	u�B	l�B	hsB	dZB	\)B	W
B	M�B	B�B	6FB	.B	'�B	$�B	�B		7B	  B��B��B��B�B�B�B�B�B�B�yB�HB�B��B��BŢB�wB�LB�-B�B��B��B�uB�hB�\B�PB�1B�B�B}�B|�By�Bx�Bx�Bv�Bv�Bt�Br�Bn�Bk�BgmBe`BbNBaHB`BB_;B^5B]/B\)B[#B[#B[#B[#B\)BZBYBYBYBW
BW
BVBVBS�BS�BS�BVBW
BW
BW
BXBXBXBYBXBXB\)B`BB`BB`BBaHBbNBcTBe`Be`BffBhsBk�Bo�Bp�Bo�Bq�Bq�Bp�Bs�Bw�B{�B|�B}�B}�B}�B}�B}�B� B�B�B�B�B�B�B�B�B�7B�JB�\B�uB��B��B��B��B��B��B�B�B�B�B�B�B�3B�dBÖBŢBƨBǮBȴB��B��B��B��B��B��B��B��B�B�B�B�B�B�)B�TB�fB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	B		7B	VB	bB	hB	uB	�B	�B	�B	�B	"�B	)�B	/B	33B	;dB	=qB	?}B	A�B	C�B	D�B	C�B	C�B	C�B	H�B	L�B	L�B	J�B	H�B	G�B	F�B	F�B	H�B	I�B	I�B	K�B	O�B	R�B	R�B	R�B	XB	YB	YB	YB	YB	ZB	^5B	`BB	aHB	cTB	ffB	hsB	iyB	k�B	l�B	l�B	l�B	m�B	l�B	l�B	n�B	p�B	o�B	p�B	q�B	t�B	t�B	u�B	t�B	u�B	v�B	y�B	{�B	}�B	� B	�B	�B	�B	�=B	�DB	�JB	�JB	�DB	�DB	�JB	�JB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BuBoBuBoBuBuBuBuBuBuBuBuBuBuBoBoBhBoBoBoBuB�B#�B1'B?}BH�BL�BO�BO�BQ�BS�BVB[#B^5B`BBbNBdZBgmBgmBcTBgmBffBiyBp�By�B~�B�B�B�B�B�B�B�B�B� B� B�7B�1B�+B�%B�By�Bn�BjBe`B_;BZBQ�BC�B=qB9XB6FB33B.B�BB��B�B�BB�B��BƨB�FB��B��B��B��B��B��B�hB�1B�B|�By�Bu�BjBD�B&�B�B  B
�sB
ȴB
�B
��B
��B
�{B
�JB
�B
{�B
o�B
cTB
^5B
ZB
VB
M�B
5?B
-B
#�B
�B
  B	�mB	�B	ȴB	�XB	�'B	�B	��B	��B	��B	�uB	�hB	�DB	�B	{�B	u�B	l�B	hsB	dZB	\)B	W
B	M�B	B�B	6FB	.B	'�B	$�B	�B		7B	  B��B��B��B�B�B�B�B�B�B�yB�HB�B��B��BŢB�wB�LB�-B�B��B��B�uB�hB�\B�PB�1B�B�B}�B|�By�Bx�Bx�Bv�Bv�Bt�Br�Bn�Bk�BgmBe`BbNBaHB`BB_;B^5B]/B\)B[#B[#B[#B[#B\)BZBYBYBYBW
BW
BVBVBS�BS�BS�BVBW
BW
BW
BXBXBXBYBXBXB\)B`BB`BB`BBaHBbNBcTBe`Be`BffBhsBk�Bo�Bp�Bo�Bq�Bq�Bp�Bs�Bw�B{�B|�B}�B}�B}�B}�B}�B� B�B�B�B�B�B�B�B�B�7B�JB�\B�uB��B��B��B��B��B��B�B�B�B�B�B�B�3B�dBÖBŢBƨBǮBȴB��B��B��B��B��B��B��B��B�B�B�B�B�B�)B�TB�fB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	B		7B	VB	bB	hB	uB	�B	�B	�B	�B	"�B	)�B	/B	33B	;dB	=qB	?}B	A�B	C�B	D�B	C�B	C�B	C�B	H�B	L�B	L�B	J�B	H�B	G�B	F�B	F�B	H�B	I�B	I�B	K�B	O�B	R�B	R�B	R�B	XB	YB	YB	YB	YB	ZB	^5B	`BB	aHB	cTB	ffB	hsB	iyB	k�B	l�B	l�B	l�B	m�B	l�B	l�B	n�B	p�B	o�B	p�B	q�B	t�B	t�B	u�B	t�B	u�B	v�B	y�B	{�B	}�B	� B	�B	�B	�B	�=B	�DB	�JB	�JB	�DB	�DB	�JB	�JB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140854                              AO  ARCAADJP                                                                    20181024140854    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140854  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140854  QCF$                G�O�G�O�G�O�0               