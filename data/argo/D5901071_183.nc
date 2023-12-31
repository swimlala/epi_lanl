CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:42Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143302  20190522121827  1727_5046_183                   2C  D   APEX                            2143                            040306                          846 @�3�@1   @�4�@
@5Q��R�c���R1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@���A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Doy�Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�fD�33D�Y�D��fD��3D�,�D�Y�D��fD��fD�&fD�Y�D�� D�� D�6fD�\�Dڙ�D��3D�#3D�Y�D�fD�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�33A33A#33AA��Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�33B�ffB�ffB�ffB���B���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffC L�C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\�C^33C`33Cb33Cd33CfL�ChL�Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D3D�3D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�3D�D��D�D��D�D��D�D��D�D��D fD ��D!�D!��D"�D"��D#�D#��D$�D$��D%3D%��D&�D&��D'�D'��D(�D(�3D)�D)��D*�D*��D+�D+��D,3D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DWfDW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��DffDf��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��DofDo�fDpfDp��Dq�Dq��Dr�Dr��Ds�Ds��Dy�3D�9�D�` D���D���D�33D�` D���D���D�,�D�` D��fD��fD�<�D�c3Dڠ D���D�)�D�` D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�ƨA�ȴA�ƨA�ƨA�A�A�ȴA�ȴA�ȴA���A���A�ĜAʾwAʼjAʼjAʺ^AʸRAʶFAʲ-Aʥ�Aʟ�Aʟ�Aʡ�Aʡ�Aʕ�Aʇ+AʅAʅA�~�A�v�A�l�A���Aȣ�A�JA�ĜA���A�A��mA�|�A���A��A��A�v�A�  A��!A�A�\)A��A���A�VA��A�^5A�?}A�33A��A�|�A�33A���A�G�A�bNA��A���A�5?A�?}A�x�A�ȴA�E�A�1'A���A�K�A��;A�VA�33A��A���A�JA��A�|�A�jA�bNA�O�A��/A��TA�9XA�~�A�p�A�A�A��RA�%A�/A� �A��PA���A���A�ȴA���A�M�A�z�A��uA�{A�-A�Q�A�O�A��!A��uA�p�A�^5A�E�A��A�PA|E�Ay�Aw�At1'Ar�!Aq\)Ao|�AnȴAn�\AlM�Aj5?AhbNAf��Ac��Ab$�A`9XA^ZA\��A\�A[�AXVAWt�AU��AT  AS��AS?}AQ�AN��AMdZAL�AK��AJbNAIhsAH{AF�jADȴAD�+AB�RA@�HA>�9A;ƨA:�A:JA7��A3�A1A0n�A.ZA,�A,jA,  A+t�A*ĜA)\)A'��A&ĜA&�A%\)A$�`A$��A$(�A"��A �A�
Ap�A��A��AdZA\)A
=A��A�AbA��A��A�hAoA��A�;A
=A9XAA�A	�;A	oAE�A�AS�A�AjA1A�!A9XA�A�-A33An�AQ�A��AoA E�@��+@���@��D@�ƨ@�;d@��!@��^@�9X@��m@���@�?}@�1@�K�@�n�@�Z@@��@�v�@�u@�=q@��;@���@�D@��@�j@�l�@��@�hs@�bN@�o@�$�@�G�@���@��@�?}@܃@��@���@��H@ڏ\@�l�@�33@���@�E�@�&�@׾w@�o@ָR@�~�@���@�o@�V@���@��@�Q�@�|�@���@ΰ!@͉7@�Z@��@�-@�J@��@ɑh@�V@���@ȴ9@�z�@�(�@ǝ�@���@�M�@š�@�G�@���@�bN@� �@�ƨ@�;d@°!@��T@��h@�x�@�7L@��@��;@��@�M�@��@�@��^@�X@��`@�bN@�1@�+@���@�M�@�x�@��u@��y@���@��@��j@��@�G�@�~�@�{@��P@�&�@��T@�@��;@�hs@�1@��j@�p�@�M�@��!@�x�@���@��T@�^5@��7@�  @�A�@��@���@�Ĝ@�Q�@�K�@���@���@�^5@��@�@���@�j@���@��y@���@�~�@�V@�v�@�E�@��9@�7L@�J@��H@���@�+@�ff@�n�@�^5@�p�@���@�9X@���@��@�~�@�E�@��@�@�@�&�@��u@�r�@�r�@�r�@�r�@�j@�Q�@�bN@�Z@�1'@���@���@�|�@�"�@��@���@��+@�5?@�J@���@���@��#@��^@�x�@�O�@�7L@��@�V@���@��@���@���@���@��@��;@���@��F@��@���@�\)@���@��T@���@���@��7@��7@�x�@�p�@�hs@�`B@�p�@�G�@�x�@�&�@��j@�I�@�j@���@���@�X@���@�dZ@��@��!@��\@�~�@��\@��y@��R@���@�G�@�G�@�O�@�/@��u@�9X@���@��@���@��F@�|�@��@���@�+@��@�b@��D@��/@���@��@���@�dZ@�+@���@�V@���@�x�@�x�@��T@��@��@�G�@�7L@�7L@���@���@���@���@+@|�@t�@i�@^{@U�-@N@Gl�@@��@8��@2��@.E�@(1'@#@?}@bN@I�@��@
-@ȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A�ƨA�ȴA�ƨA�ƨA�A�A�ȴA�ȴA�ȴA���A���A�ĜAʾwAʼjAʼjAʺ^AʸRAʶFAʲ-Aʥ�Aʟ�Aʟ�Aʡ�Aʡ�Aʕ�Aʇ+AʅAʅA�~�A�v�A�l�A���Aȣ�A�JA�ĜA���A�A��mA�|�A���A��A��A�v�A�  A��!A�A�\)A��A���A�VA��A�^5A�?}A�33A��A�|�A�33A���A�G�A�bNA��A���A�5?A�?}A�x�A�ȴA�E�A�1'A���A�K�A��;A�VA�33A��A���A�JA��A�|�A�jA�bNA�O�A��/A��TA�9XA�~�A�p�A�A�A��RA�%A�/A� �A��PA���A���A�ȴA���A�M�A�z�A��uA�{A�-A�Q�A�O�A��!A��uA�p�A�^5A�E�A��A�PA|E�Ay�Aw�At1'Ar�!Aq\)Ao|�AnȴAn�\AlM�Aj5?AhbNAf��Ac��Ab$�A`9XA^ZA\��A\�A[�AXVAWt�AU��AT  AS��AS?}AQ�AN��AMdZAL�AK��AJbNAIhsAH{AF�jADȴAD�+AB�RA@�HA>�9A;ƨA:�A:JA7��A3�A1A0n�A.ZA,�A,jA,  A+t�A*ĜA)\)A'��A&ĜA&�A%\)A$�`A$��A$(�A"��A �A�
Ap�A��A��AdZA\)A
=A��A�AbA��A��A�hAoA��A�;A
=A9XAA�A	�;A	oAE�A�AS�A�AjA1A�!A9XA�A�-A33An�AQ�A��AoA E�@��+@���@��D@�ƨ@�;d@��!@��^@�9X@��m@���@�?}@�1@�K�@�n�@�Z@@��@�v�@�u@�=q@��;@���@�D@��@�j@�l�@��@�hs@�bN@�o@�$�@�G�@���@��@�?}@܃@��@���@��H@ڏ\@�l�@�33@���@�E�@�&�@׾w@�o@ָR@�~�@���@�o@�V@���@��@�Q�@�|�@���@ΰ!@͉7@�Z@��@�-@�J@��@ɑh@�V@���@ȴ9@�z�@�(�@ǝ�@���@�M�@š�@�G�@���@�bN@� �@�ƨ@�;d@°!@��T@��h@�x�@�7L@��@��;@��@�M�@��@�@��^@�X@��`@�bN@�1@�+@���@�M�@�x�@��u@��y@���@��@��j@��@�G�@�~�@�{@��P@�&�@��T@�@��;@�hs@�1@��j@�p�@�M�@��!@�x�@���@��T@�^5@��7@�  @�A�@��@���@�Ĝ@�Q�@�K�@���@���@�^5@��@�@���@�j@���@��y@���@�~�@�V@�v�@�E�@��9@�7L@�J@��H@���@�+@�ff@�n�@�^5@�p�@���@�9X@���@��@�~�@�E�@��@�@�@�&�@��u@�r�@�r�@�r�@�r�@�j@�Q�@�bN@�Z@�1'@���@���@�|�@�"�@��@���@��+@�5?@�J@���@���@��#@��^@�x�@�O�@�7L@��@�V@���@��@���@���@���@��@��;@���@��F@��@���@�\)@���@��T@���@���@��7@��7@�x�@�p�@�hs@�`B@�p�@�G�@�x�@�&�@��j@�I�@�j@���@���@�X@���@�dZ@��@��!@��\@�~�@��\@��y@��R@���@�G�@�G�@�O�@�/@��u@�9X@���@��@���@��F@�|�@��@���@�+@��@�b@��D@��/@���@��@���@�dZ@�+@���@�V@���@�x�@�x�@��T@��@��@�G�@�7L@�7L@���@���@���@���@+@|�@t�@i�@^{@U�-@N@Gl�@@��@8��@2��@.E�@(1'@#@?}@bN@I�@��@
-@ȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�FB�XB�dB�XB�^B�dB�jB�qB�qB�dB�wB�wB�}B�}B�}B�}B�}B��B��B��B��BB�RBÖBÖBÖBÖBĜBĜBĜBŢBĜBB��B��B�B�
B��BɺB�^B��B��BYB�'B�dB��B�qB�XB�dB��B�B��B��B��B��BŢB�dB�B�3B�'B��B��B��B��B�1Bn�Be`B8RB=qB-B1'B2-B'�B,B)�B%�B�B�B�B�B�BbB1B��B�B�fB�B��B�FB��B��B�JB~�Bu�BffBM�B;dB�BB
�)B
�^B
��B
��B
y�B
dZB
q�B
n�B
k�B
dZB
T�B
33B
�B
DB	��B	�B	�B	�
B	��B	ŢB	��B	�RB	��B	��B	�VB	{�B	w�B	m�B	bNB	W
B	Q�B	L�B	E�B	>wB	8RB	,B	$�B	"�B	�B	�B	\B		7B	%B	B��B��B�B�yB�yB�sB�ZB�/B�B��B��B��BĜB�dB�LB�-B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�bB�bB�bB�bB�bB�\B�JB�7B�%B�B|�Bs�Bp�Bo�Bn�Bm�Bl�Bm�Bl�Bl�Bl�BjBjBjBiyBhsBgmBgmBgmBgmBjBhsBhsBiyBl�Bm�Bl�Bl�Bo�Bv�Bu�Bx�By�By�Bz�B{�B{�Bz�Bz�Bx�By�Bu�Bt�Bw�Bx�By�By�B|�Bz�Bx�By�B|�B}�B�B�%B�+B�=B�hB�oB�oB�hB�uB��B��B��B��B��B��B��B��B��B��B�B�!B�B�B�!B�3B�?B�9B�LB�dB�}BÖBÖBĜBŢBǮBȴBȴBɺB��B��B��B��B�B�B�B�)B�/B�;B�NB�`B�yB�B�B�B�B�B��B��B��B��B	B	B	B	%B	+B		7B	
=B	DB	PB	\B	VB	%B	B	  B	B��B�B��B	+B	�B	"�B	)�B	&�B	�B	�B	#�B	(�B	,B	0!B	.B	0!B	7LB	;dB	:^B	9XB	=qB	B�B	D�B	H�B	L�B	L�B	L�B	M�B	N�B	Q�B	W
B	T�B	R�B	S�B	VB	YB	[#B	[#B	]/B	]/B	\)B	e`B	jB	p�B	jB	gmB	jB	m�B	o�B	p�B	q�B	u�B	w�B	{�B	}�B	�B	�%B	�=B	�PB	�PB	�bB	�oB	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�'B	�'B	�!B	�!B	�!B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�9B	�XB	�dB	�jB	�dB	�dB	�dB	�}B	ĜB	ɺB	ɺB	ǮB	B	��B	�}B	�}B	�}B	�}B	B	��B	��B	��B	B	B	��B	��B	B	ĜB	ƨB	ǮB	ƨB	ƨB	ŢB	ƨB	��B	��B	��B	�B	�B	�B	�
B	�
B	�
B	�B	�B	��B	��B	��B	�B	�B	�)B	�/B	�5B	�;B	�BB	�`B	�ZB	�TB	�mB	�B
\B
�B
�B
�B
&�B
/B
8RB
>wB
G�B
N�B
S�B
YB
_;B
hsB
m�B
q�B
t�B
y�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�FB�XB�dB�XB�^B�dB�jB�qB�qB�dB�wB�wB�}B�}B�}B�}B�}B��B��B��B��BB�RBÖBÖBÖBÖBĜBĜBĜBŢBĜBĜBƨB��B�B�#B�B��BB��B��Be`B�9B�qB��B�}B�dB�jB��B�B��B��B��B��BƨB�wB�3B�FB�3B�B��B��B��B�VBx�Bo�BF�BD�B5?B49B5?B+B-B+B'�B�B�B�B�B�BhBDB��B�B�yB�BĜB�RB�B��B�bB�Bx�BjBS�BA�B%�BJB
�fB
ȴB
��B
��B
�DB
gmB
r�B
o�B
k�B
e`B
ZB
=qB
#�B
hB
B	��B	�/B	�B	��B	ƨB	��B	�jB	��B	��B	�oB	�B	z�B	r�B	gmB	[#B	S�B	O�B	L�B	A�B	=qB	1'B	%�B	#�B	%�B	�B	uB	
=B		7B		7B��B��B�B�B�B�B�yB�NB�#B��B��B��B��B�wB�^B�LB�3B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�bB�hB�hB�uB�uB�VB�DB�7B�B�%B{�Bs�Bs�Bq�Bp�Bn�Bn�Bn�Bn�Bn�Bo�Bl�Bk�Bk�BjBjBhsBiyBjBm�Bk�BjBk�Bm�Bn�Bm�Bn�Bq�Bv�Bv�Bz�B{�Bz�B{�B~�B|�B{�Bz�B{�B|�Bx�Bx�Bx�Bx�By�B{�B~�B{�Bz�B{�B}�B~�B�B�%B�+B�DB�oB�uB�{B�hB�oB��B��B��B��B��B��B��B��B��B�B�!B�'B�!B�!B�'B�9B�FB�FB�XB�qB��BÖBÖBŢBƨBǮBȴBȴBɺB��B��B��B��B�
B�B�#B�)B�5B�BB�TB�fB�B�B�B�B�B��B��B��B��B��B	B	B	%B	%B		7B	
=B	DB	JB	\B	oB	oB	
=B	B	B	B��B�B�B	B	�B	"�B	.B	,B	!�B	�B	!�B	&�B	,B	2-B	/B	.B	7LB	=qB	=qB	8RB	<jB	B�B	D�B	H�B	N�B	M�B	L�B	N�B	O�B	Q�B	YB	VB	T�B	T�B	VB	YB	[#B	[#B	]/B	_;B	[#B	dZB	iyB	s�B	l�B	hsB	jB	m�B	p�B	q�B	r�B	v�B	x�B	|�B	}�B	�B	�%B	�=B	�VB	�VB	�bB	�oB	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�'B	�'B	�'B	�'B	�!B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�9B	�XB	�dB	�qB	�jB	�jB	�dB	�wB	ÖB	ɺB	��B	��B	ÖB	��B	�}B	�}B	�}B	�wB	B	ÖB	��B	��B	B	B	B	B	B	ĜB	ƨB	ǮB	ƨB	ǮB	ŢB	ŢB	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�
B	�B	��B	��B	�B	�B	�)B	�5B	�5B	�;B	�;B	�fB	�`B	�TB	�mB	�B
\B
�B
�B
�B
&�B
/B
8RB
>wB
G�B
N�B
S�B
YB
_;B
gmB
m�B
q�B
t�B
y�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447382012010314473820120103144738  AO  ARGQ                                                                        20111130143302  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143302  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144738  IP                  G�O�G�O�G�O�                