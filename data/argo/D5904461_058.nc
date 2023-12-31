CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-28T19:18:51Z AOML 3.0 creation; 2016-08-07T21:36:36Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150828191851  20160807143636  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               :A   AO  5286_8897_058                   2C  D   APEX                            6531                            072314                          846 @�]o�1   @�]����@1��+J�cV5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    :A   B   B   @9��@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�fD�fD�I�D�l�D���D�	�D�<�D��fD�� D�fD�33D��fD�ٚD���D�)�Dڜ�D�ٚD�3D�<�D�l�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Ffg@�ff@�ffA��A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B	33B33B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffBܙ�B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CXL�CZL�C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DgD��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD3DD��DE�DE�3DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt� Dy�3D��D�P D�s3D��3D� D�C3D���D��fD��D�9�D���D�� D�3D�0 Dڣ3D�� D��D�C3D�s3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�%A���A֮A֏\A���A��HA���A��;A��yA��`Aե�A���A���A�9XA��TA�;dA�A�Q�A��AП�A�$�A�E�A΋DA�K�A���ÃA���A���A�-A��Aɝ�A��AȾwA�bNA�33A���A��`A�E�Aư!AƩ�A�5?A�K�A���A�%A�{A���A���A��wA��A��A��yA�JA��^A���A�-A��A�ȴA���A��7A�bA�
=A�JA�  A��yA���A���A�^5A�9XA�
=A�l�A���A��uA�`BA�l�A��+A��\A��A�{A�dZA��uA�(�A�A���A�9XA���A�n�A�E�A�-A� �A�/A��A���A�A�A�JA�&�A���A��hA���A��wA�|�A��wA���A��9A��uA��/A��A|��A{XAxbNAt�jAp��An�\Ak��Ah5?AdffAa�
A[AV�uAS�AQdZAN��AK��AH^5AE�
AD-A?�A>=qA=�A;hsA:�+A9�A7�^A5�A4I�A3p�A2��A21A1�FA1ƨA1t�A0�A0bNA/O�A.5?A-�7A-;dA-oA,z�A+�mA+oA(ĜA'��A&I�A%�^A%��A%hsA$(�A"Q�A ��A��AȴA��A�A��A|�A��A(�A�uA�-Al�A;dAȴAA�A�
A�^A�7A~�A�TA`BA33A�FA�;A~�A��A=qA�;A�TAȴAbNA+AdZA5?A(�A�-A^5A
-A	�;A	�#A��AA
=A��A��A�uAA�AAhsA��A�FAp�A��A�!AffAZAbNA-A j@���@�@�p�@�V@�V@���@�33@���@���@��
@��-@�Q�@띲@땁@�F@�\)@�
=@��
@�j@���@�E�@�33@�\)@�=q@�R@��;@�@�+@���@�5?@�{@��@�ƨ@��@ޏ\@�p�@܋D@��
@�K�@�v�@���@�|�@֗�@�J@���@�G�@���@�Q�@ӶF@�t�@�K�@�S�@�
=@�V@���@���@�1@��m@�  @�\)@�E�@�M�@�@�K�@�t�@��
@��m@Η�@�5?@͙�@�X@���@��
@˝�@��@���@�ƨ@�
=@��@��@��@��;@���@�Ĝ@�l�@�;d@��@Η�@�ff@�5?@���@�O�@�l�@��y@�^5@ȋD@�dZ@�l�@��@��@��F@��@�@��@��H@�~�@�$�@�J@��@���@��@�bN@�ƨ@�
=@�ȴ@�^5@�J@��@��9@�j@�j@�j@�bN@�(�@��w@���@���@���@�p�@���@�j@���@��9@��@�I�@�(�@��;@��;@��m@���@�Ĝ@�7L@�G�@��h@��#@���@�p�@�x�@�G�@���@��@�j@�1@�  @�  @�ƨ@�|�@�+@�ȴ@��\@�~�@�v�@�ff@��@�p�@��
@�o@�@�V@�5?@��#@�/@��`@��@��u@�r�@�1@���@��@��\@�$�@���@�@���@���@�?}@�%@��D@�Q�@�b@�1@��;@�t�@��y@��H@���@�5?@���@��h@���@�r�@�A�@��m@�|�@�+@�o@���@��@�?}@�j@�9X@�  @���@��;@��F@�t�@�o@���@���@���@�ff@��#@���@��@���@�b@�b@��@��
@��@�+@��H@��\@��-@��@��@�p�@�p�@�V@�r�@�1'@���@��w@��P@�\)@�+@��@�ȴ@��R@�{@��#@���@���@��h@�hs@�&�@���@�z�@�I�@�(�@�1@��;@��F@�;d@��y@��H@���@�-@�p�@�7L@��@��@��7@�;d@~@s�m@i�#@a�^@Z�!@Q�@J��@A��@;�
@5?}@-O�@(  @#t�@ff@�@K�@dZ@�T111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�p�A�%A���A֮A֏\A���A��HA���A��;A��yA��`Aե�A���A���A�9XA��TA�;dA�A�Q�A��AП�A�$�A�E�A΋DA�K�A���ÃA���A���A�-A��Aɝ�A��AȾwA�bNA�33A���A��`A�E�Aư!AƩ�A�5?A�K�A���A�%A�{A���A���A��wA��A��A��yA�JA��^A���A�-A��A�ȴA���A��7A�bA�
=A�JA�  A��yA���A���A�^5A�9XA�
=A�l�A���A��uA�`BA�l�A��+A��\A��A�{A�dZA��uA�(�A�A���A�9XA���A�n�A�E�A�-A� �A�/A��A���A�A�A�JA�&�A���A��hA���A��wA�|�A��wA���A��9A��uA��/A��A|��A{XAxbNAt�jAp��An�\Ak��Ah5?AdffAa�
A[AV�uAS�AQdZAN��AK��AH^5AE�
AD-A?�A>=qA=�A;hsA:�+A9�A7�^A5�A4I�A3p�A2��A21A1�FA1ƨA1t�A0�A0bNA/O�A.5?A-�7A-;dA-oA,z�A+�mA+oA(ĜA'��A&I�A%�^A%��A%hsA$(�A"Q�A ��A��AȴA��A�A��A|�A��A(�A�uA�-Al�A;dAȴAA�A�
A�^A�7A~�A�TA`BA33A�FA�;A~�A��A=qA�;A�TAȴAbNA+AdZA5?A(�A�-A^5A
-A	�;A	�#A��AA
=A��A��A�uAA�AAhsA��A�FAp�A��A�!AffAZAbNA-A j@���@�@�p�@�V@�V@���@�33@���@���@��
@��-@�Q�@띲@땁@�F@�\)@�
=@��
@�j@���@�E�@�33@�\)@�=q@�R@��;@�@�+@���@�5?@�{@��@�ƨ@��@ޏ\@�p�@܋D@��
@�K�@�v�@���@�|�@֗�@�J@���@�G�@���@�Q�@ӶF@�t�@�K�@�S�@�
=@�V@���@���@�1@��m@�  @�\)@�E�@�M�@�@�K�@�t�@��
@��m@Η�@�5?@͙�@�X@���@��
@˝�@��@���@�ƨ@�
=@��@��@��@��;@���@�Ĝ@�l�@�;d@��@Η�@�ff@�5?@���@�O�@�l�@��y@�^5@ȋD@�dZ@�l�@��@��@��F@��@�@��@��H@�~�@�$�@�J@��@���@��@�bN@�ƨ@�
=@�ȴ@�^5@�J@��@��9@�j@�j@�j@�bN@�(�@��w@���@���@���@�p�@���@�j@���@��9@��@�I�@�(�@��;@��;@��m@���@�Ĝ@�7L@�G�@��h@��#@���@�p�@�x�@�G�@���@��@�j@�1@�  @�  @�ƨ@�|�@�+@�ȴ@��\@�~�@�v�@�ff@��@�p�@��
@�o@�@�V@�5?@��#@�/@��`@��@��u@�r�@�1@���@��@��\@�$�@���@�@���@���@�?}@�%@��D@�Q�@�b@�1@��;@�t�@��y@��H@���@�5?@���@��h@���@�r�@�A�@��m@�|�@�+@�o@���@��@�?}@�j@�9X@�  @���@��;@��F@�t�@�o@���@���@���@�ff@��#@���@��@���@�b@�b@��@��
@��@�+@��H@��\@��-@��@��@�p�@�p�@�V@�r�@�1'@���@��w@��P@�\)@�+@��@�ȴ@��R@�{@��#@���@���@��h@�hs@�&�@���@�z�@�I�@�(�@�1@��;@��F@�;d@��y@��H@���@�-@�p�@�7LG�O�@��@��7@�;d@~@s�m@i�#@a�^@Z�!@Q�@J��@A��@;�
@5?}@-O�@(  @#t�@ff@�@K�@dZ@�T111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
D�B
C�B
A�B
A�B
>wB
:^B
9XB
8RB
=qB
A�B
B�B
L�B
o�B
��B
�wB
ȴB
��B
�
B
�TB
�B  BoB$�B8RBXBaHBP�BYB��B��B�/BB�B�B(�B2-B@�BL�BQ�BQ�Bx�B�oB��B��Bk�B#�B
=BPB��B9XBK�BT�BhsBffBN�BF�BVBcTBjBiyBdZBz�BŢB�B�)B�BB��B�B�;B�;B��B�dB�B�B��B�wB�TB�B�B��B�TBB�3B��B��B��B�XB�!B��B�\By�BK�BB�B�{BhsBP�B49B�B
�B
��B
��B
e`B
G�B
uB	�ZB	�B	��B	�}B	��B	�%B	v�B	bNB	K�B	7LB	+B	�B	\B��B�yB�TB�HB�/B�/B�#B��BB�qB�XB�RB�FB�?B�9B�FB��BƨB��B��B�B�;B�;B�5B�;B�TB�ZB�ZB�TB�TB�NB�BB�/B�B��B��B��B�B��B��B��BŢBŢBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�fB��B�B�B	B	�B	�B	�B	+B	B	bB	)�B	/B	2-B	+B	�B	�B	(�B	$�B	$�B	$�B	+B	,B	2-B	5?B	49B	1'B	-B	(�B	'�B	&�B	&�B	-B	8RB	<jB	8RB	.B	"�B	!�B	!�B	�B	{B	{B	oB	uB	�B	{B	bB	hB	{B	�B	�B	�B	�B	$�B	-B	<jB	D�B	R�B	ZB	XB	M�B	F�B	A�B	=qB	=qB	=qB	=qB	>wB	=qB	>wB	>wB	>wB	>wB	=qB	>wB	C�B	C�B	A�B	A�B	H�B	L�B	K�B	N�B	R�B	T�B	W
B	YB	[#B	[#B	_;B	cTB	`BB	^5B	^5B	`BB	`BB	`BB	cTB	k�B	p�B	u�B	{�B	}�B	{�B	|�B	{�B	{�B	z�B	x�B	x�B	|�B	�hB	��B	��B	��B	��B	��B	�B	�LB	�dB	�RB	�^B	�^B	�XB	�XB	�dB	�dB	�jB	�RB	�LB	�9B	�B	��B	��B	�oB	�bB	�bB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�XB	�XB	�XB	�XB	�^B	�wB	�}B	B	ĜB	��B	��B	��B	�B	�)B	�/B	�/B	�;B	�BB	�BB	�;B	�BB	�BB	�TB	�TB	�ZB	�ZB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�HB	�/B	�)B	�)B	�)B	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
1B
hB
�B
�B
#�B
)�B
-B
2-B
9XB
?}B
F�B
L�B
P�B
W
B
[#B
^5B
dZB
iyB
n�B
q�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
D�B
C�B
A�B
A�B
>pB
:SB
9PB
8JB
=lB
AB
B�B
L�B
o�B
��B
�lB
ȧB
��B
��B
�GB
�B
��B_B$�B8BBXBa7BP�BYB��B��B�B�BqB�B(�B2B@pBL�BQ�BQ�Bx�B�`B��B�wBkuB#�B
-B@B��B9EBK�BT�BhbBfXBN�BF�BU�BcBBjpBiiBdHBz�BŒB�B�B�vB�B��B�qB�-B�*B��B�RB��B��B��B�gB�EB�B�B��B�DBB�%B��B��B��B�GB�B��B�JBy�BK�BB�B�gBhbBP�B4#B�B
�B
��B
�vB
eTB
G�B
kB	�OB	�B	��B	�sB	��B	�B	v�B	bFB	K�B	7GB	*�B	�B	WB��B�vB�PB�CB�*B�+B�B��BB�lB�UB�QB�DB�<B�6B�DB��BƤBʾB��B�B�7B�6B�1B�6B�PB�UB�UB�PB�NB�IB�?B�*B�B��B��B��B��B��B��BʽBŞBŞBǨBɶBʽB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�cB��B�B�B	B	�B	�B	B	"B	B	]B	)�B	/B	2"B	*�B	�B	�B	(�B	$�B	$�B	$�B	*�B	+�B	2#B	55B	4.B	1B	-B	(�B	'�B	&�B	&�B	-B	8HB	<`B	8HB	.	B	"�B	!�B	!�B	�B	oB	qB	gB	kB	|B	oB	VB	]B	pB	xB	�B	�B	�B	$�B	-B	<\B	D�B	R�B	ZB	XB	M�B	F�B	A�B	=dB	=eB	=eB	=gB	>iB	=eB	>kB	>jB	>jB	>kB	=bB	>hB	C�B	C�B	AB	AB	H�B	L�B	K�B	N�B	R�B	T�B	V�B	Y
B	[B	[B	_,B	cHB	`6B	^(B	^(B	`3B	`4B	`4B	cGB	kxB	p�B	u�B	{�B	}�B	{�B	|�B	{�B	{�B	z�B	x�B	x�B	|�B	�YB	��B	��B	��B	��B	��B	�
B	�=B	�RB	�AB	�OB	�PB	�GB	�FB	�RB	�SB	�ZB	�?B	�:B	�(B	�B	��B	��B	�`B	�RB	�TB	�eB	�lB	�mB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�#B	�GB	�GB	�FB	�GB	�MB	�hB	�lB	�|B	ĉB	��B	��B	��B	�B	�B	�B	�B	�(B	�1B	�0B	�)B	�1B	�2B	�DB	�DB	�HB	�IB	�BB	�BB	�AB	�CB	�CB	�AB	�CB	�5B	�B	�B	�B	�B	�6B	�<B	�BB	�IB	�GB	�FB	�FB	�NB	�OB	�MB	�MB	�LB	�SB	�\B	�\B	�[B	�XB	�ZB	�aB	�iB	�lB	�hB	�mB	�qB	�yB	�xB	�{B	�B	�~B	�B	�B	�}B	�B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
 �B
�B
�B
�B
�B
�B
�B
�G�O�B
 B
SB
�B
�B
#�B
)�B
,�B
2B
9DB
?gB
F�B
L�B
P�B
V�B
[B
^B
dCB
ibB
n�B
q�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436362016080714363620160807143636  AO  ARCAADJP                                                                    20150828191851    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150828191851  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150828191851  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143636  IP                  G�O�G�O�G�O�                