CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-20T20:18:08Z AOML 3.0 creation; 2016-08-07T21:36:42Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151220201808  20160807143642  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ZA   AO  5286_8897_090                   2C  D   APEX                            6531                            072314                          846 @ׇ��	�1   @ׇ��8��@3?;dZ��cG
=p��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ZA   B   B   @�33@�33A��A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�ffB���B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy` D��D�L�D��3D�� D��D�FfD��3D���D��D�0 D�l�D���D��D�0 Dډ�D��D�	�D�,�D�3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@�(�A{A z�A@z�A`z�A�=qA�=qA�=qA�p�A�=qA�=qA�=qA�=qB �B�B�RB�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bw�RB�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�\B�\B�B�B�u�B���B�\B��)B�\B�\B�\B�\B�\B�\B�\B�B�B�u�B��)B�\B��)B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C!�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'�RD(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@{�DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dya�D��D�M�D��)D���D��D�G\D��)D���D��D�0�D�m�D���D��D�0�Dڊ�D��D�
�D�-�D�)D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��;A��/A��;A��;A��;A��;A��;A��HA��HA��/A��`A��yA��yA��yA��yA��A��A��A��A��A��A���A���A��A��A���A���A���A���A���A���A���A�A��A�$�A�1'A�K�A�^5A�hsA�jA�t�Aϕ�Aϩ�Aϣ�A��A˩�A�;dA� �A�bNA�$�A�ĜA�ĜA��`A�O�A�t�A��A�(�A��A�oA��A��jA�ĜA���A��A��A��A���A�oA��+A��A���A�ƨA�JA��A��A��A���A�1A�?}A�=qA�t�A��A�5?A� �A��HA��RA�bNA�^5A�?}A���A��/A��\A�hsA��A�\)A���A�ffA���A���A��yA���A���A��RA��A}��A{
=AxJAv�ArI�AodZAm|�AlȴAl  Ai
=AdI�AbE�A_VA\5?AZ��AY�AWx�AT�ANAJAGXAF{AC|�AA�A@��A@z�A@=qA??}A>I�A=�-A=7LA<(�A8��A7p�A5x�A4  A2�!A1��A0�!A.��A-��A,��A,bA+33A)�7A'�A'&�A&�/A&v�A&M�A&(�A%�^A$v�A#�;A#"�A!�TA ��AO�A��A=qAG�An�A(�AJA�DAVAJA��A�;A�mA��AbA{A{AXA�7A?}A�A�yA��A&�A
��A
ĜA
��A��A��A
��A�A�A�RA33A|�A �HA {@��
@��;A Q�A"�A��AXA�yA�A+A {@���@�Ĝ@��@�-@�  @��@�ƨ@���@�1@��@�?}@ܣ�@�33@�J@؋D@���@�+@�x�@�%@Ԭ@�Z@�b@��@��@ҟ�@�%@���@�;d@�M�@�`B@�X@��`@�ƨ@���@�@�dZ@˾w@˶F@��@˥�@�r�@���@��
@�r�@�1'@\@��@��h@���@�5?@�$�@��@�
=@�^5@�@�G�@�r�@�1'@�1@�  @��w@�;d@��@��@��@�G�@�7L@��h@�
=@���@��!@�-@���@�9X@��!@�@��`@��@�Q�@�z�@�1'@��F@���@�Z@�+@��@���@��u@�V@��/@�r�@��/@���@�z�@��F@���@�{@�O�@��D@�(�@��@���@�&�@�
=@��D@�ƨ@�b@��@��/@���@��9@��@���@�\)@�"�@���@���@�O�@�t�@�M�@���@�X@��H@�t�@���@�l�@�M�@���@���@��9@�{@�t�@�;d@�ȴ@���@�X@��@���@��9@��!@�hs@��@��@���@�ƨ@��
@��@���@�p�@���@��^@��;@��P@��R@��#@�?}@���@�Q�@�A�@��@��@�r�@�I�@�b@���@�K�@�ƨ@�K�@�M�@�J@�hs@��@�Q�@��P@�;d@�;d@���@���@���@�ff@�=q@�E�@��#@�@���@�7L@�&�@��@��`@���@�%@���@�bN@��;@��@��w@�l�@�l�@�t�@�t�@��H@�?}@���@�  @��@���@��w@��@�b@�|�@�@���@��P@��@�5?@���@�x�@�hs@�?}@�?}@�`B@�7L@��j@���@��@�r�@�Q�@�Q�@��m@�33@���@���@�M�@�{@��@���@���@���@��h@�/@��@�9X@�1@�  @��@�\)@�33@�
=@�S�@�\)@���@��\@�ȴ@�v�@�$�@�J@�@���@�V@���@�j@��@�  @��
@���@��w@�dZ@�\)@�t�@�33@���@��@�ȴ@�n�@�@���@��h@���@��@��7@~ff@t�@kS�@c@[dZ@S"�@L�@D��@<�@6V@0�9@*��@#C�@�@Z@��@��@M�@@	&�@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��;A��/A��;A��;A��;A��;A��;A��HA��HA��/A��`A��yA��yA��yA��yA��A��A��A��A��A��A���A���A��A��A���A���A���A���A���A���A���A�A��A�$�A�1'A�K�A�^5A�hsA�jA�t�Aϕ�Aϩ�Aϣ�A��A˩�A�;dA� �A�bNA�$�A�ĜA�ĜA��`A�O�A�t�A��A�(�A��A�oA��A��jA�ĜA���A��A��A��A���A�oA��+A��A���A�ƨA�JA��A��A��A���A�1A�?}A�=qA�t�A��A�5?A� �A��HA��RA�bNA�^5A�?}A���A��/A��\A�hsA��A�\)A���A�ffA���A���A��yA���A���A��RA��A}��A{
=AxJAv�ArI�AodZAm|�AlȴAl  Ai
=AdI�AbE�A_VA\5?AZ��AY�AWx�AT�ANAJAGXAF{AC|�AA�A@��A@z�A@=qA??}A>I�A=�-A=7LA<(�A8��A7p�A5x�A4  A2�!A1��A0�!A.��A-��A,��A,bA+33A)�7A'�A'&�A&�/A&v�A&M�A&(�A%�^A$v�A#�;A#"�A!�TA ��AO�A��A=qAG�An�A(�AJA�DAVAJA��A�;A�mA��AbA{A{AXA�7A?}A�A�yA��A&�A
��A
ĜA
��A��A��A
��A�A�A�RA33A|�A �HA {@��
@��;A Q�A"�A��AXA�yA�A+A {@���@�Ĝ@��@�-@�  @��@�ƨ@���@�1@��@�?}@ܣ�@�33@�J@؋D@���@�+@�x�@�%@Ԭ@�Z@�b@��@��@ҟ�@�%@���@�;d@�M�@�`B@�X@��`@�ƨ@���@�@�dZ@˾w@˶F@��@˥�@�r�@���@��
@�r�@�1'@\@��@��h@���@�5?@�$�@��@�
=@�^5@�@�G�@�r�@�1'@�1@�  @��w@�;d@��@��@��@�G�@�7L@��h@�
=@���@��!@�-@���@�9X@��!@�@��`@��@�Q�@�z�@�1'@��F@���@�Z@�+@��@���@��u@�V@��/@�r�@��/@���@�z�@��F@���@�{@�O�@��D@�(�@��@���@�&�@�
=@��D@�ƨ@�b@��@��/@���@��9@��@���@�\)@�"�@���@���@�O�@�t�@�M�@���@�X@��H@�t�@���@�l�@�M�@���@���@��9@�{@�t�@�;d@�ȴ@���@�X@��@���@��9@��!@�hs@��@��@���@�ƨ@��
@��@���@�p�@���@��^@��;@��P@��R@��#@�?}@���@�Q�@�A�@��@��@�r�@�I�@�b@���@�K�@�ƨ@�K�@�M�@�J@�hs@��@�Q�@��P@�;d@�;d@���@���@���@�ff@�=q@�E�@��#@�@���@�7L@�&�@��@��`@���@�%@���@�bN@��;@��@��w@�l�@�l�@�t�@�t�@��H@�?}@���@�  @��@���@��w@��@�b@�|�@�@���@��P@��@�5?@���@�x�@�hs@�?}@�?}@�`B@�7L@��j@���@��@�r�@�Q�@�Q�@��m@�33@���@���@�M�@�{@��@���@���@���@��h@�/@��@�9X@�1@�  @��@�\)@�33@�
=@�S�@�\)@���@��\@�ȴ@�v�@�$�@�J@�@���@�V@���@�j@��@�  @��
@���@��w@�dZ@�\)@�t�@�33@���@��@�ȴ@�n�@�@���@��h@���@��G�O�@~ff@t�@kS�@c@[dZ@S"�@L�@D��@<�@6V@0�9@*��@#C�@�@Z@��@��@M�@@	&�@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
bB
uB
�B
$�B
-B
33B
6FB
=qB
N�B
cTB
k�B
�!BƨBBF�Be`Be`BcTBcTBdZBq�By�B�7B��B�FB�^B��B�)B�TB�yB�B�B�/B��B��BǮB��B�?B��B��B�PB}�Bt�Bk�BaHBC�B"�B��B�
B��B��B�?B��B�DBl�B[#BK�B9XB"�BbB
��B
�'B
�B
��B
k�B
W
B
:^B
$�B
{B
B	�B	��B	�qB	��B	��B	�JB	�JB	�1B	�B	}�B	l�B	T�B	E�B	49B	"�B	�B	bB	B�B��BŢB�jB�LB�'B�B�B�B�B�B�B�B�B��B��B��B�oB�PB�VB��B�hB��B�hB�\B�PB�=B�=B�JB�bB�uB��B��B�{B�uB�bB�JB�1B�=B�1B�=B�PB�PB�PB�uB�{B�uB��B��B��B�B�B�!B�'B�3B�9B�9B�!B��B�VB�B�Bu�Bt�Bv�Bz�B�B��B��B��B�=B|�Bo�BaHBT�BN�BM�BP�BS�BcTBy�B�%BɺB��B��B��B�'B��B�bBjB\)BVBN�BI�BG�BC�B>wB<jB:^B8RB8RB:^BI�BVBW
BYBZBYBZB\)B_;B_;B_;B`BBcTBe`BhsBiyBjBq�B�B�=B�PB�\B�hB��B�B�-B�dB��B�{B�uB�oB��B��B��B��B��B��B�B�B��B��B�B�!B�9B�?B�FB�RB�dB�wB��BÖBǮB��B�#B�B��B		7B��B�mB�TB�HB�BB�ZB�mB�B�B�B��B��B	uB	�B	�B	VB	B	B	B	
=B	VB	bB	\B	JB	JB	PB	\B	\B	uB	�B	#�B	49B	1'B	2-B	6FB	.B	0!B	1'B	9XB	J�B	cTB	VB	E�B	A�B	E�B	I�B	[#B	]/B	bNB	\)B	W
B	_;B	m�B	z�B	|�B	|�B	m�B	jB	s�B	|�B	�hB	��B	��B	��B	�B	�!B	�!B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�?B	�9B	�3B	�'B	�'B	�3B	�9B	�FB	�wB	�wB	��B	��B	��B	ĜB	��B	��B	��B	��B	��B	ȴB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�#B	�)B	�;B	�BB	�TB	�TB	�NB	�HB	�NB	�ZB	�`B	�`B	�`B	�ZB	�`B	�HB	�;B	�BB	�BB	�BB	�NB	�ZB	�fB	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
1B
	7B
1B
	7B
	7B

=B
	7B
JB
DB
�B
 �B
$�B
.B
6FB
;dB
B�B
I�B
M�B
Q�B
XB
_;B
bNB
ffB
hsB
k�B
o�B
s�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
:B
:B
8B
:B
8B
8B
:B
8B
8B
8B
:B
:B
8B
:B
8B
1B
0B
1B
1B
0B
0B
1B
4B
3B
3B
4B
1B
4B
4B
:B
9B
7B
7B
hB
|B
�B
$�B
-B
37B
6LB
=tB
N�B
cXB
k�B
�#BƦBBF�Be]Be]BcQBcTBdVBq�By�B�5B��B�EB�_B��B�'B�PB�wB�B�B�.B��BʿBǭB��B�=B��B��B�OB}�Bt�Bk�BaDBC�B"�B��B�B��BʿB�>B��B�ABl�B[BK�B9UB"�BbB
��B
�'B
�B
��B
k�B
WB
:bB
$�B
B
B	�B	�B	�xB	��B	��B	�RB	�TB	�:B	�(B	}�B	l�B	UB	E�B	4GB	"�B	�B	qB	'B�B�BųB�zB�ZB�9B�B� B�B�B�B�&B�&B�B��B��B��B��B�aB�hB��B�{B��B�xB�pB�dB�PB�OB�\B�sB��B��B��B��B��B�sB�]B�DB�NB�AB�PB�cB�bB�cB��B��B��B��B��B��B�B�*B�1B�4B�DB�IB�IB�/B��B�fB�'B�Bu�Bt�Bv�Bz�B�&B��B�B��B�PB|�Bo�Ba]BUBN�BM�BP�BTBcgBy�B�7B��B� B��B��B�6B��B�uBj�B\<BVBN�BI�BG�BC�B>�B<}B:sB8fB8iB:rBI�BVBWBY+BZ0BY(BZ.B\<B_NB_OB_MB`UBcfBeqBh�Bi�Bj�Bq�B�$B�MB�aB�oB�wB��B�B�:B�qB��B��B��B�B��B��B��B��B��B��B�B�B��B��B�B�.B�HB�OB�UB�_B�qB��B��BåBǻB��B�0B�B��B		CB��B�zB�cB�TB�NB�gB�zB�B�B�B��B�B	~B	�B	�B	`B	B	B	B	
GB	`B	mB	eB	UB	VB	^B	fB	fB	�B	�B	#�B	4AB	10B	24B	6NB	.B	0+B	12B	9^B	J�B	c\B	VB	E�B	A�B	E�B	I�B	[,B	]9B	bUB	\/B	WB	_DB	m�B	z�B	|�B	|�B	m�B	j�B	s�B	|�B	�mB	��B	��B	�B	�B	�&B	�(B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�>B	�CB	�=B	�8B	�,B	�+B	�7B	�>B	�KB	�|B	�|B	��B	��B	��B	ĢB	��B	��B	��B	��B	��B	ȸB	ǲB	ǲB	ȸB	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�&B	�-B	�'B	�,B	�=B	�FB	�YB	�XB	�SB	�KB	�TB	�\B	�eB	�fB	�aB	�^B	�dB	�MB	�=B	�DB	�EB	�EB	�PB	�]B	�iB	�eB	�lB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
 B

B
B
B
B
B
B
B
(B
5B
3B
3B
4B
4B
	:B
4B
	;B
	9B

?B
	<G�O�B
FB
�B
 �B
$�B
.B
6FB
;fB
B�B
I�B
M�B
Q�B
XB
_:B
bNB
feB
hrB
k�B
o�B
s�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.03 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436422016080714364220160807143642  AO  ARCAADJP                                                                    20151220201808    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151220201808  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151220201808  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143642  IP                  G�O�G�O�G�O�                