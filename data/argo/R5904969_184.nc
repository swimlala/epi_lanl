CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:14:39Z creation      
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
resolution        =���   axis      Z        ,  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ?�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     ,  A(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  GT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     ,  H�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  O   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  U8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  V�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  ^|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  d�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  j�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  l`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  zD   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    zt   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    }t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20181024141439  20181024141439  5904969 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6784                            2B  A   APEX                            7725                            111215                          846 @��$���1   @��%@yoL@2�"��`B�cƧ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @,��@y��@���A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� D�T�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @1�@~�R@�\)AG�A"�HAAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch.Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
=C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C��pC�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD��DD�DD�DD�DD�DD�DD�D��D�DD�DD�DD~�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,�D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�D�W\D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴAȾwA�|�A��A�=qA�  A�ĜA�ZA��`A�Aş�AőhAŏ\Aŏ\Aŏ\AœuAř�Aš�Aũ�Aŕ�Aŗ�Aŧ�A��mA��AōPA�|�A�^5A�XA�S�A�Q�A�O�A�7LA�
=Aİ!A�K�A�jA�
=AA�Q�A�%A��#A��9A�M�A�l�A���A���A�l�A���A��#A�M�A�$�A�VA�1A��A���A��+A�bA���A� �A�\)A���A�bA��\A�O�A��HA�E�A�%A���A��A��yA�\)A��!A�A�A�A�ȴA��wA��uA�XA�S�A�7LA�+A�XA��A�n�A��A�"�A�I�A�"�A���A���A�
=A�I�A���A���A�M�A�{A��A��mA��TA�A�VA�|�A� �A��A���A�l�A|5?As��An�uAjQ�Ahv�Af1'Ac�
Ab��Aa�wA`bNA`�A[AW�^AV  AR��AQAK�#AI"�AG�PAGC�AF�/AD�ABn�A@v�A?��A?l�A>�\A>E�A>(�A=�-A<�A9;dA6�!A5��A5x�A4��A3C�A2A�A0A�A/��A.r�A-��A,�!A+�PA*��A*JA)
=A'�^A&��A%��A%t�A%K�A$��A#�-A!��AƨAbNA �A�A{A��A=qAbAK�A��A��A�A
�DA	��A�/AoA�
A�
A;dA1'Ax�A��@���@��u@���@��y@�V@���@�p�@��@���@�j@���@�;d@��T@�1@��H@�v�@�^5@�V@�M�@�-@�9@�dZ@��y@��T@�S�@��@�^@�/@��/@�bN@�l�@�h@�!@��@�7@�&�@�bN@�\)@�ȴ@�V@��@���@��/@�^5@��/@�ƨ@�@�@�Z@�33@Ұ!@�@�%@�(�@ϝ�@�\)@�\)@�G�@��@�ƨ@��@˾w@��@���@���@�bN@�bN@�bN@��m@���@��;@�ƨ@��@��#@�&�@Ĭ@�bN@�9X@��m@�dZ@§�@�v�@�M�@���@��@��`@��j@�Z@� �@��
@�dZ@��R@�M�@��@�J@�@���@��@�Ĝ@�Q�@��@��@���@�33@��!@��\@�E�@�$�@�@���@��@�hs@��@���@��u@��u@�z�@�j@��@��@�ƨ@��F@��@�K�@��\@���@���@��D@�Q�@�1@��@�l�@�
=@��!@��\@��+@�n�@�$�@�@�O�@���@�I�@��w@�
=@��R@��+@�M�@�$�@�{@���@��-@���@��h@�O�@��/@��D@�Q�@�9X@�  @�t�@�+@�
=@�ȴ@�~�@�v�@��+@���@��!@���@���@��@���@��@��/@��@�A�@� �@�b@�ƨ@�|�@�33@�
=@���@�~�@��@�@�@�V@���@��u@��@�I�@��@�\)@���@u2a11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴAȾwA�|�A��A�=qA�  A�ĜA�ZA��`A�Aş�AőhAŏ\Aŏ\Aŏ\AœuAř�Aš�Aũ�Aŕ�Aŗ�Aŧ�A��mA��AōPA�|�A�^5A�XA�S�A�Q�A�O�A�7LA�
=Aİ!A�K�A�jA�
=AA�Q�A�%A��#A��9A�M�A�l�A���A���A�l�A���A��#A�M�A�$�A�VA�1A��A���A��+A�bA���A� �A�\)A���A�bA��\A�O�A��HA�E�A�%A���A��A��yA�\)A��!A�A�A�A�ȴA��wA��uA�XA�S�A�7LA�+A�XA��A�n�A��A�"�A�I�A�"�A���A���A�
=A�I�A���A���A�M�A�{A��A��mA��TA�A�VA�|�A� �A��A���A�l�A|5?As��An�uAjQ�Ahv�Af1'Ac�
Ab��Aa�wA`bNA`�A[AW�^AV  AR��AQAK�#AI"�AG�PAGC�AF�/AD�ABn�A@v�A?��A?l�A>�\A>E�A>(�A=�-A<�A9;dA6�!A5��A5x�A4��A3C�A2A�A0A�A/��A.r�A-��A,�!A+�PA*��A*JA)
=A'�^A&��A%��A%t�A%K�A$��A#�-A!��AƨAbNA �A�A{A��A=qAbAK�A��A��A�A
�DA	��A�/AoA�
A�
A;dA1'Ax�A��@���@��u@���@��y@�V@���@�p�@��@���@�j@���@�;d@��T@�1@��H@�v�@�^5@�V@�M�@�-@�9@�dZ@��y@��T@�S�@��@�^@�/@��/@�bN@�l�@�h@�!@��@�7@�&�@�bN@�\)@�ȴ@�V@��@���@��/@�^5@��/@�ƨ@�@�@�Z@�33@Ұ!@�@�%@�(�@ϝ�@�\)@�\)@�G�@��@�ƨ@��@˾w@��@���@���@�bN@�bN@�bN@��m@���@��;@�ƨ@��@��#@�&�@Ĭ@�bN@�9X@��m@�dZ@§�@�v�@�M�@���@��@��`@��j@�Z@� �@��
@�dZ@��R@�M�@��@�J@�@���@��@�Ĝ@�Q�@��@��@���@�33@��!@��\@�E�@�$�@�@���@��@�hs@��@���@��u@��u@�z�@�j@��@��@�ƨ@��F@��@�K�@��\@���@���@��D@�Q�@�1@��@�l�@�
=@��!@��\@��+@�n�@�$�@�@�O�@���@�I�@��w@�
=@��R@��+@�M�@�$�@�{@���@��-@���@��h@�O�@��/@��D@�Q�@�9X@�  @�t�@�+@�
=@�ȴ@�~�@�v�@��+@���@��!@���@���@��@���@��@��/@��@�A�@� �@�b@�ƨ@�|�@�33@�
=@���@�~�@��@�@�@�V@���@��u@��@�I�@��@�\)@���@u2a11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B9XB9XB9XB9XB9XB9XB9XB9XB9XB9XB9XB9XB:^B:^B:^B9XB:^B;dB>wBF�BB�B9XB33B+B!�B�B�B�B�B�B�B!�B#�B%�B(�B&�B&�B1'BL�Bu�B�JB�DB�VB�\B�bB�bB�hB��B��B�B�9B��BƨB��B��B��B��B��B�B�HB�ZB�B+B�B)�B5?B8RB:^B;dB<jB;dBB�BC�BT�BVBW
BVBT�BT�BT�BXBXBW
BW
BVBVBQ�BN�BK�BJ�BG�B;dB33B49B8RB9XBC�BF�BJ�BK�BE�BA�B@�B+B�B��B�B�B�LB��B�{Bw�B!�B
�`B
�3B
�bB
m�B
B�B
&�B
hB	�B	ÖB	��B	w�B	K�B	=qB	8RB	9XB	&�B	�B	JB	+B	"�B	oB	B��B�TB�5B��B��BƨBƨBƨB�wB�3B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�9B�3B�3B�'B�'B�B�!B�B�B�B�B��B�B��B��B�B�B�B�B�B�B�B�B�B�'B�B�'B�!B�B�B�B��B��B��B��B��B�B�'B�'B�'B�'B�-B�-B�3B�3B�9B�9B�3B�3B�?B�LB�RB�RB�RB�RB�RB�dB�qB�qB�qB�jB�}B�wB�wB�wB�wB�wB�qB�qB�^B�^B�dB�qB�}B��BÖBƨBȴB��B�
B�)B�#B�#B�B�B�B�B�B�B�#B�)B�;B�BB�HB�B�B�B��B��B	B	%B	+B	+B	%B	+B	
=B		7B		7B		7B	1B	PB	bB	uB	�B	�B	�B	#�B	'�B	(�B	)�B	-B	2-B	33B	49B	9XB	<jB	=qB	@�B	F�B	G�B	H�B	H�B	H�B	J�B	L�B	M�B	O�B	O�B	Q�B	R�B	T�B	YB	ZB	\)B	]/B	^5B	dZB	e`B	e`B	gmB	hsB	gmB	gmB	hsB	hsB	iyB	iyB	jB	iyB	iyB	jB	k�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	p�B	r�B	r�B	t�B	v�B	x�B	{�B	}�B	~�B	� B	�B	�+B	�7B	�DB	�JB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�'B	�?B	�dB	��B	��B	B	ÖB	ÖB	ŢB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
B
 '11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B9XB9XB9XB9XB9XB9XB9XB9XB9XB9XB9XB9XB:^B:^B:^B9XB:^B;dB>wBF�BB�B9XB33B+B!�B�B�B�B�B�B�B!�B#�B%�B(�B&�B&�B1'BL�Bu�B�JB�DB�VB�\B�bB�bB�hB��B��B�B�9B��BƨB��B��B��B��B��B�B�HB�ZB�B+B�B)�B5?B8RB:^B;dB<jB;dBB�BC�BT�BVBW
BVBT�BT�BT�BXBXBW
BW
BVBVBQ�BN�BK�BJ�BG�B;dB33B49B8RB9XBC�BF�BJ�BK�BE�BA�B@�B+B�B��B�B�B�LB��B�{Bw�B!�B
�`B
�3B
�bB
m�B
B�B
&�B
hB	�B	ÖB	��B	w�B	K�B	=qB	8RB	9XB	&�B	�B	JB	+B	"�B	oB	B��B�TB�5B��B��BƨBƨBƨB�wB�3B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�9B�3B�3B�'B�'B�B�!B�B�B�B�B��B�B��B��B�B�B�B�B�B�B�B�B�B�'B�B�'B�!B�B�B�B��B��B��B��B��B�B�'B�'B�'B�'B�-B�-B�3B�3B�9B�9B�3B�3B�?B�LB�RB�RB�RB�RB�RB�dB�qB�qB�qB�jB�}B�wB�wB�wB�wB�wB�qB�qB�^B�^B�dB�qB�}B��BÖBƨBȴB��B�
B�)B�#B�#B�B�B�B�B�B�B�#B�)B�;B�BB�HB�B�B�B��B��B	B	%B	+B	+B	%B	+B	
=B		7B		7B		7B	1B	PB	bB	uB	�B	�B	�B	#�B	'�B	(�B	)�B	-B	2-B	33B	49B	9XB	<jB	=qB	@�B	F�B	G�B	H�B	H�B	H�B	J�B	L�B	M�B	O�B	O�B	Q�B	R�B	T�B	YB	ZB	\)B	]/B	^5B	dZB	e`B	e`B	gmB	hsB	gmB	gmB	hsB	hsB	iyB	iyB	jB	iyB	iyB	jB	k�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	p�B	r�B	r�B	t�B	v�B	x�B	{�B	}�B	~�B	� B	�B	�+B	�7B	�DB	�JB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�'B	�?B	�dB	��B	��B	B	ÖB	ÖB	ŢB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
B
 '11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141439                              AO  ARCAADJP                                                                    20181024141439    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141439  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141439  QCF$                G�O�G�O�G�O�0               