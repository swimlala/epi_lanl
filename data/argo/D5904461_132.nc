CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-07-29T08:00:53Z AOML 3.0 creation; 2016-08-07T21:36:49Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160729080053  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286_8897_132                   2C  D   APEX                            6531                            072314                          846 @׿"�=1   @׿���@5�/��w�c;dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D�fD�<�D�vfD��3D��fD�0 D��fD�ɚD���D�0 D���D��3D�3D�C3D�vfD�ٚD�3D�6fD�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
=@��
A�A!�AA�Aa�A���A���A���A���A���A���A���A���B z�Bz�Bz�Bz�B z�B(z�B0z�B8z�B@z�BHz�BPz�BXz�B`z�Bhz�Bpz�Bxz�B�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C8RC�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C��C�\C�\C�\C�\C�\D �D �HD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM�HDN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�DyǮD�
=D�@�D�z=D��
D��=D�3�D��=D��qD��D�3�D���D��
D�
D�G
D�z=D��qD�
D�:=D�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�ffA�ffA�ffA�\)A���A͋DÁA̓AͅA͇+A�z�A�n�AʾwAȾwA�O�A���A�bNA���AžwA�jA��A�I�A�|�A�"�A�VA¼jA�v�A�5?A�+A���A��TA��^A�v�A��#A�;dA��A�$�A���A���A��yA��A�\)A���A��wA�p�A��A���A���A�S�A�1A��
A�VA�A��/A�?}A� �A��uA��A��;A�n�A�A���A��A��RA�?}A��RA�(�A�n�A��A��A�oA��A��
A��uA��A��A���A�(�A���A���A��`A�~�A� �A���A�33A�(�A�C�A���A�1A��A�r�A���A�
=A�`BA�1A���A�hsA��wA�E�A���A���A�XA��A�^A}&�Ay��AxVAu��As�
Aq+Am�wAln�AkAi33AgO�Af��Ad��Abn�AahsA`ĜA`JA^jA]�#A\��A[x�AY�FAX5?AW�AU�PAR�9AP�yANr�AL^5AJ��AIO�AH9XAG�;AEG�AB�RAA+A@$�A?;dA=hsA;ƨA: �A7�hA5�;A5C�A3x�A2{A1��A0��A/�;A/��A/�A-�;A,�A,5?A+��A+7LA+C�A+oA*��A(��A&��A%\)A$��A$ffA$9XA$JA#A!�^A�TAS�A9XA+AI�AXAĜAS�Ar�A�A�AO�A �AA�A�Al�A�+A��A  A\)A�HAjA��A��A�`Al�A
(�A
�+A	+AI�A?}AAA�A�mA�A�AK�A ��A �uA  �@�K�@�`B@��@�$�@�A�@�`B@�Q�@�w@��@��@��@�|�@홚@��@��@�t�@�-@�33@���@��@�hs@�hs@�1'@���@�hs@�?}@�+@��@�V@ڸR@�p�@�7L@�b@�V@�
=@�v�@щ7@У�@�A�@�S�@·+@͑h@�%@̼j@�j@�^5@��@�dZ@ɺ^@��@ȓu@�Q�@�I�@�Z@�Z@�\)@�o@���@�ff@�^5@Ɨ�@�{@�v�@Ƈ+@Ƈ+@�n�@ũ�@��@Ĵ9@ģ�@�j@��
@�;d@���@�ff@��@��m@�
=@�
=@��y@��H@��H@��!@�o@��@�Q�@��u@�@��@���@�9X@��T@�x�@�hs@�@��m@��/@�ƨ@��@�-@�ƨ@�;d@�|�@�C�@�|�@��@�~�@��@�V@�ƨ@�+@�;d@��F@��@��
@���@�+@��\@��@���@��@�Ĝ@�9X@���@�S�@�33@�@��@���@�J@��h@�p�@�V@��@��j@��@�A�@���@�|�@�ȴ@��!@�^5@�@�O�@���@�A�@���@���@��w@��@�@���@���@��+@�v�@��@�x�@��@�/@��@���@�7L@��`@�Q�@�j@��@� �@���@���@��H@���@�n�@�-@��@���@��7@�G�@�Ĝ@�Q�@�1@�b@�1'@�1'@�Q�@��@�j@�A�@��@��@��
@�C�@���@�M�@�-@��@�J@��@�&�@�bN@�Z@�A�@�A�@�1@��@��@���@���@�V@���@�G�@�hs@�O�@�&�@���@���@�Ĝ@�r�@�A�@���@���@��P@�|�@�K�@�n�@�{@���@�7L@�&�@��@���@��9@�Z@��@�  @��w@�C�@�
=@��@�v�@�@���@�p�@�7L@�V@��9@�bN@�1'@� �@��w@���@�S�@�33@���@��\@�5?@�$�@�J@�@��#@��-@��@���@��@�bN@�1'@��@��@��
@���@��F@���@���@���@�t�@�dZ@�S�@�`B@�O�@|�j@q��@j-@a�@[��@T��@M�-@GK�@B~�@=��@7�w@2-@-O�@'�@!��@�j@ �@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A�dZA�ffA�ffA�ffA�\)A���A͋DÁA̓AͅA͇+A�z�A�n�AʾwAȾwA�O�A���A�bNA���AžwA�jA��A�I�A�|�A�"�A�VA¼jA�v�A�5?A�+A���A��TA��^A�v�A��#A�;dA��A�$�A���A���A��yA��A�\)A���A��wA�p�A��A���A���A�S�A�1A��
A�VA�A��/A�?}A� �A��uA��A��;A�n�A�A���A��A��RA�?}A��RA�(�A�n�A��A��A�oA��A��
A��uA��A��A���A�(�A���A���A��`A�~�A� �A���A�33A�(�A�C�A���A�1A��A�r�A���A�
=A�`BA�1A���A�hsA��wA�E�A���A���A�XA��A�^A}&�Ay��AxVAu��As�
Aq+Am�wAln�AkAi33AgO�Af��Ad��Abn�AahsA`ĜA`JA^jA]�#A\��A[x�AY�FAX5?AW�AU�PAR�9AP�yANr�AL^5AJ��AIO�AH9XAG�;AEG�AB�RAA+A@$�A?;dA=hsA;ƨA: �A7�hA5�;A5C�A3x�A2{A1��A0��A/�;A/��A/�A-�;A,�A,5?A+��A+7LA+C�A+oA*��A(��A&��A%\)A$��A$ffA$9XA$JA#A!�^A�TAS�A9XA+AI�AXAĜAS�Ar�A�A�AO�A �AA�A�Al�A�+A��A  A\)A�HAjA��A��A�`Al�A
(�A
�+A	+AI�A?}AAA�A�mA�A�AK�A ��A �uA  �@�K�@�`B@��@�$�@�A�@�`B@�Q�@�w@��@��@��@�|�@홚@��@��@�t�@�-@�33@���@��@�hs@�hs@�1'@���@�hs@�?}@�+@��@�V@ڸR@�p�@�7L@�b@�V@�
=@�v�@щ7@У�@�A�@�S�@·+@͑h@�%@̼j@�j@�^5@��@�dZ@ɺ^@��@ȓu@�Q�@�I�@�Z@�Z@�\)@�o@���@�ff@�^5@Ɨ�@�{@�v�@Ƈ+@Ƈ+@�n�@ũ�@��@Ĵ9@ģ�@�j@��
@�;d@���@�ff@��@��m@�
=@�
=@��y@��H@��H@��!@�o@��@�Q�@��u@�@��@���@�9X@��T@�x�@�hs@�@��m@��/@�ƨ@��@�-@�ƨ@�;d@�|�@�C�@�|�@��@�~�@��@�V@�ƨ@�+@�;d@��F@��@��
@���@�+@��\@��@���@��@�Ĝ@�9X@���@�S�@�33@�@��@���@�J@��h@�p�@�V@��@��j@��@�A�@���@�|�@�ȴ@��!@�^5@�@�O�@���@�A�@���@���@��w@��@�@���@���@��+@�v�@��@�x�@��@�/@��@���@�7L@��`@�Q�@�j@��@� �@���@���@��H@���@�n�@�-@��@���@��7@�G�@�Ĝ@�Q�@�1@�b@�1'@�1'@�Q�@��@�j@�A�@��@��@��
@�C�@���@�M�@�-@��@�J@��@�&�@�bN@�Z@�A�@�A�@�1@��@��@���@���@�V@���@�G�@�hs@�O�@�&�@���@���@�Ĝ@�r�@�A�@���@���@��P@�|�@�K�@�n�@�{@���@�7L@�&�@��@���@��9@�Z@��@�  @��w@�C�@�
=@��@�v�@�@���@�p�@�7L@�V@��9@�bN@�1'@� �@��w@���@�S�@�33@���@��\@�5?@�$�@�J@�@��#@��-@��@���@��@�bN@�1'@��@��@��
@���@��F@���@���@���@�t�@�dZG�O�@�`B@�O�@|�j@q��@j-@a�@[��@T��@M�-@GK�@B~�@=��@7�w@2-@-O�@'�@!��@�j@ �@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�/B
�5B
�/B
�)B
��BjB�B�!B�!B�!B�!B�B��B�\BǮB��BBhB�B(�B9XBF�BZBk�Bn�Bn�Bo�Bp�Br�Br�Bv�Bw�B{�B�\B��BȴBɺB��B��B�B�B�RB��B�7B�\B��B�B�B�FB�^B�fB%BJB1B��B��B��B�B�ZB�B��B��B��BB�B��B�VBy�BZBM�BI�B49B"�BoBuB1B��B�yB�/B�B��B�#B��B��BǮB��B��B�BffB]/BT�BP�BG�B&�BbB
��B
�fB
�dB
�B
��B
}�B
iyB
J�B
6FB
"�B
bB	��B	�B	�)B	��B	�RB	��B	�oB	�+B	{�B	p�B	k�B	aHB	S�B	K�B	G�B	B�B	<jB	9XB	2-B	)�B	�B	uB	JB	+B��B�B�mB�/B�B��B��B��B�}B�-B�B��B��B��B��B�hB�VB�7B�1B�+B�B�VB�7B�+B�+B�B{�Bx�Bw�Bx�B{�B�B�%B�By�Bl�BjBiyBiyBhsBgmBe`BcTBbNB`BB`BB^5B[#BZBW
BW
BXBXBW
BW
BW
BZB^5BffBk�Bp�By�B�B�B�B�+B�1B�7B�Bw�B� B{�By�Bw�Bs�Bk�Bk�BjBffB`BBhsBn�Bq�Bp�Bp�Bp�Bn�Bk�BgmBe`BffBffBn�Bo�Bu�Br�Bv�Bu�Bs�Bm�BjBl�Bl�Bo�B|�Bz�Bn�BgmBx�B�bB�hB�\B�oB�bB�\B�JB�%B�B�B�B�B�B�%B�7B�DB�JB�PB�bB�B�^B�3B�'B�3B�9B�XB�qB�}BÖBɺB��B��B�B�B�HB�fB�B�B�B�B�B��B	  B	B	B	B	B	+B		7B	1B	
=B	DB	hB	�B	�B	�B	"�B	'�B	;dB	;dB	>wB	I�B	F�B	7LB	7LB	D�B	C�B	F�B	A�B	H�B	Q�B	Q�B	Q�B	N�B	G�B	J�B	N�B	P�B	XB	^5B	bNB	bNB	aHB	^5B	aHB	dZB	iyB	p�B	r�B	s�B	u�B	y�B	{�B	z�B	z�B	{�B	{�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�+B	�DB	�\B	�hB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�9B	�9B	�LB	�LB	�LB	�RB	�RB	�^B	�}B	�}B	�qB	�wB	��B	��B	B	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�5B	�BB	�HB	�HB	�TB	�ZB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�fB	�`B	�fB	�mB	�sB	�mB	�sB	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
\B
�B
�B
$�B
(�B
/B
5?B
9XB
=qB
C�B
H�B
O�B
P�B
W
B
^5B
bNB
ffB
k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B
�)B
�+B
�)B
�!B
��BjsB�B�B�B�B�B�B��B�PBǥB��BB]B�B(�B9OBF�BZBk�Bn�Bn�Bo�Bp�Br�Br�Bv�Bw�B{�B�RB��BȭBɰB��B��B�B�B�JB�wB�-B�QB��B�
B�B�>B�VB�\BBFB*B��B��B��B�B�TB��B��B��B��BB�
B��B�MBy�BZBM�BI�B4/B"�BfBjB#B��B�oB�"B��B��B�B��B��BǥB�}B��B��Bf\B]#BT�BP�BG�B&�BXB
��B
�^B
�[B
�B
�xB
}�B
irB
J�B
6AB
"�B
`B	��B	�B	�'B	��B	�QB	��B	�mB	�+B	{�B	p�B	k�B	aIB	S�B	K�B	G�B	B�B	<kB	9ZB	20B	* B	�B	wB	MB	.B��B�B�qB�4B�B�B��B��B��B�5B�B�B��B��B��B�rB�]B�@B�9B�3B�&B�^B�>B�5B�4B�"B{�Bx�Bw�Bx�B{�B�"B�,B�'By�Bl�Bj�Bi�Bi�Bh�BgyBekBc\BbVB`KB`MB^>B[/BZ'BWBWBXBXBWBWBWBZ%B^?BfoBk�Bp�By�B�!B�B�B�3B�8B�?B� Bw�B�B{�By�Bw�Bs�Bk�Bk�Bj�BfnB`JBh}Bn�Bq�Bp�Bp�Bp�Bn�Bk�BgwBehBfpBfnBn�Bo�Bu�Br�Bv�Bu�Bs�Bm�Bj�Bl�Bl�Bo�B|�Bz�Bn�BgtBx�B�hB�mB�bB�tB�hB�bB�NB�)B�B�B�B�B�B�*B�9B�KB�NB�TB�gB�B�_B�7B�'B�7B�<B�YB�sB��BÚBɽB��B��B�B�B�IB�fB�B�B�B�B�B��B��B	B	B	B	B	*B		7B	1B	
>B	CB	iB	�B	�B	�B	"�B	'�B	;cB	;cB	>uB	I�B	F�B	7JB	7HB	D�B	C�B	F�B	A�B	H�B	Q�B	Q�B	Q�B	N�B	G�B	J�B	N�B	P�B	XB	^2B	bKB	bHB	aEB	^2B	aEB	dVB	itB	p�B	r�B	s�B	u�B	y�B	{�B	z�B	z�B	{�B	{�B	|�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�&B	�%B	�>B	�XB	�bB	�hB	�lB	�oB	�wB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�-B	�1B	�0B	�CB	�EB	�DB	�JB	�KB	�WB	�tB	�tB	�jB	�qB	�~B	��B	B	ĕB	ƢB	ʻB	ʻB	˿B	˾B	��B	��B	˿B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�	B	�B	�B	�B	�B	�B	�&B	�(B	�-B	�/B	�:B	�AB	�AB	�LB	�QB	�LB	�PB	�RB	�SB	�PB	�QB	�OB	�[B	�XB	�^B	�fB	�kB	�fB	�lB	�sB	�mB	�qB	�qB	�uB	�uB	�uB	�uB	�}B	�}B	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��G�O�B	��B
B
RB
�B
�B
$�B
(�B
/B
53B
9OB
=fB
C�B
H�B
O�B
P�B
V�B
^(B
bEB
f\B
ky11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436492016080714364920160807143649  AO  ARCAADJP                                                                    20160729080053    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160729080053  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160729080053  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143649  IP                  G�O�G�O�G�O�                