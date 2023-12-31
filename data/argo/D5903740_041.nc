CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:26Z AOML 3.0 creation; 2016-06-01T00:08:12Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230826  20160531170812  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               )A   AO  4055_7112_041                   2C  D   APEX                            5374                            041511                          846 @֓�/hP1   @̼֓�o�@:.V�u�c_"��`B1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    )A   A   A   @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDty�Dy` D�	�D�I�D�� D��fD�  D�P D���D��fD�3D�C3D�y�D�� D��D�<�Dڃ3D��D�fD�@ D�c3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @5@�z�@�z�A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B(�\B0�\B8�\B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D\D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV�\DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt\Dt��Dyh�D�D�ND��{D���D�{D�T{D��D���D��D�G�D�~D��{D�HD�AHDڇ�D��HD��D�D{D�g�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�A�
=A�1A�A���A���A�?}A��A��-A�K�A��FA�r�A�l�A���A�VA�?}A��^A�/A���A��hA�jA�$�A�ȴA���A�|�A�33A��A��uA�ZA�bA��#A���A�`BA�9XA���A�A�A��A�A���A��
A�"�A�E�A�{A��A�z�A�jA�A�A�M�A�+A���A��TA�A�~�A�ZA�^5A�G�A�/A��A���A��-A��A���A���A�r�A�`BA�XA�K�A�/A���A��TA��-A�ffA�C�A�&�A�%A��yA��#A�r�A�9XA�%A��^A���A���A�ffA�bA�A��`A���A��mA��HA�{A���A�{A��PA��DA��^A��TA�/A�ƨA��7A�A�A�JA��PA���A�
A}�
Az�uAw�
Au�Aqx�Ao��An(�Aj��Ah��Af�DAcl�Aap�A^��A\��AZ�AY��AX �AW�7AV�9AVE�AU��AUoAS��AP�ANffAN1'AN(�AM�mAL��AK��AI��AI;dAH��AFȴAE�FAD9XABVAA�A@z�A>r�A<�!A<  A;A:�RA9��A8ȴA81A7x�A6�A5�A4~�A3��A2�uA2{A1A1�A/G�A.ZA-�mA-��A-O�A,��A,v�A+��A+�A*�A*A)p�A)�A(��A'�^A&�+A%C�A$��A#�A!O�A ĜA (�A�FA�A�A�A�hA�9AbNA1A�A
=A�9A$�A�^A�A-A�DA�-A��A\)A��A-A�7AZA�Az�A��A�7A7LAA
��A
ffA	�
A9XAt�Az�AhsA+AVA�hA�FA`BAG�A r�@���@��@�E�@���@���@��y@�X@�J@�l�@�5?@�@��`@�dZ@�~�@���@�7@�1'@�S�@ꗍ@�p�@�(�@�V@�^@�Ĝ@�1@�;d@�E�@��T@��@�V@�r�@��@�C�@��y@�=q@��/@��@ۅ@�o@�ȴ@�$�@�@ؓu@�  @�@�j@���@ӍP@Л�@��@ͩ�@͙�@�p�@�%@�dZ@�Ĝ@�
=@�&�@�  @��y@��@�`B@��@���@��+@��@�|�@��y@��R@��+@�@�bN@��@��R@��^@��9@�9X@�o@��@��-@�X@���@�bN@��@�"�@��!@�n�@�V@�-@�@�O�@�9X@��@��@�$�@��j@���@�ȴ@�x�@��@���@���@�Ĝ@�Ĝ@��@�Q�@���@���@�S�@�$�@�X@�Ĝ@�9X@��m@�dZ@�v�@�J@��j@� �@���@�|�@��H@�ff@�@��7@�x�@�/@��@���@��@�t�@�
=@�ȴ@���@�v�@���@�X@�&�@���@�Z@� �@���@�C�@��y@�ȴ@�n�@�5?@���@�`B@��9@�I�@��w@�S�@�33@�o@���@��+@�5?@��@�p�@��j@��@�r�@�Z@�1'@��@���@�;d@�o@��H@��R@�v�@�-@�{@�@���@��@��7@�%@���@��D@�bN@�A�@�b@��@��@�dZ@�K�@�;d@�+@��@�@���@���@�^5@�-@�J@��^@��h@�x�@�x�@�hs@�G�@���@���@�Z@�1'@�b@���@��
@��@�dZ@�33@��@�ȴ@���@�~�@�v�@�V@�$�@��@�p�@�?}@�7L@���@��9@��D@�;@�w@��@l�@K�@+@~�@~5?@}��@}��@}`B@|�j@|j@|I�@{��@{ƨ@{��@{C�@{33@{@z�!@z�\@zn�@zM�@zM�@z=q@y�#@x��@x�u@x�u@x�@x �@w�w@w|�@v��@r��@l(�@d�/@]�@V�+@O+@HbN@C�@=�@7\)@1��@-��@'��@ 1'@j@��@��@�@@b@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�A�
=A�1A�A���A���A�?}A��A��-A�K�A��FA�r�A�l�A���A�VA�?}A��^A�/A���A��hA�jA�$�A�ȴA���A�|�A�33A��A��uA�ZA�bA��#A���A�`BA�9XA���A�A�A��A�A���A��
A�"�A�E�A�{A��A�z�A�jA�A�A�M�A�+A���A��TA�A�~�A�ZA�^5A�G�A�/A��A���A��-A��A���A���A�r�A�`BA�XA�K�A�/A���A��TA��-A�ffA�C�A�&�A�%A��yA��#A�r�A�9XA�%A��^A���A���A�ffA�bA�A��`A���A��mA��HA�{A���A�{A��PA��DA��^A��TA�/A�ƨA��7A�A�A�JA��PA���A�
A}�
Az�uAw�
Au�Aqx�Ao��An(�Aj��Ah��Af�DAcl�Aap�A^��A\��AZ�AY��AX �AW�7AV�9AVE�AU��AUoAS��AP�ANffAN1'AN(�AM�mAL��AK��AI��AI;dAH��AFȴAE�FAD9XABVAA�A@z�A>r�A<�!A<  A;A:�RA9��A8ȴA81A7x�A6�A5�A4~�A3��A2�uA2{A1A1�A/G�A.ZA-�mA-��A-O�A,��A,v�A+��A+�A*�A*A)p�A)�A(��A'�^A&�+A%C�A$��A#�A!O�A ĜA (�A�FA�A�A�A�hA�9AbNA1A�A
=A�9A$�A�^A�A-A�DA�-A��A\)A��A-A�7AZA�Az�A��A�7A7LAA
��A
ffA	�
A9XAt�Az�AhsA+AVA�hA�FA`BAG�A r�@���@��@�E�@���@���@��y@�X@�J@�l�@�5?@�@��`@�dZ@�~�@���@�7@�1'@�S�@ꗍ@�p�@�(�@�V@�^@�Ĝ@�1@�;d@�E�@��T@��@�V@�r�@��@�C�@��y@�=q@��/@��@ۅ@�o@�ȴ@�$�@�@ؓu@�  @�@�j@���@ӍP@Л�@��@ͩ�@͙�@�p�@�%@�dZ@�Ĝ@�
=@�&�@�  @��y@��@�`B@��@���@��+@��@�|�@��y@��R@��+@�@�bN@��@��R@��^@��9@�9X@�o@��@��-@�X@���@�bN@��@�"�@��!@�n�@�V@�-@�@�O�@�9X@��@��@�$�@��j@���@�ȴ@�x�@��@���@���@�Ĝ@�Ĝ@��@�Q�@���@���@�S�@�$�@�X@�Ĝ@�9X@��m@�dZ@�v�@�J@��j@� �@���@�|�@��H@�ff@�@��7@�x�@�/@��@���@��@�t�@�
=@�ȴ@���@�v�@���@�X@�&�@���@�Z@� �@���@�C�@��y@�ȴ@�n�@�5?@���@�`B@��9@�I�@��w@�S�@�33@�o@���@��+@�5?@��@�p�@��j@��@�r�@�Z@�1'@��@���@�;d@�o@��H@��R@�v�@�-@�{@�@���@��@��7@�%@���@��D@�bN@�A�@�b@��@��@�dZ@�K�@�;d@�+@��@�@���@���@�^5@�-@�J@��^@��h@�x�@�x�@�hs@�G�@���@���@�Z@�1'@�b@���@��
@��@�dZ@�33@��@�ȴ@���@�~�@�v�@�V@�$�@��@�p�@�?}@�7L@���@��9@��D@�;@�w@��@l�@K�@+@~�@~5?@}��@}��@}`B@|�j@|j@|I�@{��@{ƨ@{��@{C�@{33@{@z�!@z�\@zn�@zM�@zM�@z=q@y�#@x��@x�u@x�u@x�@x �@w�w@w|�@v��@r��@l(�@d�/@]�@V�+@O+@HbN@C�@=�@7\)@1��@-��@'��@ 1'@j@��@��@�@@b@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�FB�FB�FB�FB�FB�?B�?B�9B�XBŢB�BB�#B��BĜBȴB�}B��B�XB�B�B��B��B��B��B�RB�LB�?B�9B�!B�B��B��B��B��B��B�DB\)BG�B1'B1'BB�BVB_;B\)BL�BP�BQ�BR�BaHB^5BYBYBVBN�BK�BO�BO�BM�BH�BE�BC�BB�BB�BA�B>wB=qB?}B=qB:^B5?B2-B-B&�B"�B�B�B�B�BPB%B  B��B�BBƨB�?B��B�BN�B&�B��B�TB��BĜB��B�%BE�B+B
��B
�!B
��B
��B
��B
r�B
XB
H�B
)�B
�B	��B	�`B	��B	�LB	��B	��B	�B	s�B	e`B	P�B	B�B	2-B	%�B	�B	�B	\B	JB	1B	%B	B	  B��B�B�TB�TB�TB�NB�5B�
B��B��B��B��BȴB��B�RB�?B�'B��B��B��B��B��B��B��B��B��B��B�{B�oB�VB�=B�7B�+B�%B�B�B� B� B�B�B�B�B�B�B~�B|�Bz�By�B|�Bz�By�Bv�Bq�BjBhsBffBffBe`BdZBaHB]/B\)B[#BZBXBW
BVBT�BR�BN�BH�BE�BB�B>wB;dB9XB7LB49B2-B0!B.B-B,B,B+B)�B'�B$�B!�B�B�B�B�B�B�B�B�BuBuBoBhBbB\B\BVBDB
=B
=B	7B	7B1B+B+B+B%B%B%B%BBB%BBBBB%B%BBBBBBBB%B+B+B+B+B+B%B+B%B+B	7B	7B+B
=B\B\B\B\BVBbB{B�B�B�B�B�B�B�B�B#�B%�B(�B+B+B+B+B.B0!B1'B33B6FB6FB8RB;dB<jB<jB=qB>wB@�BA�BC�BC�BC�BC�BD�BE�BH�BI�BK�BL�BQ�BVBYB^5B`BB`BB`BB`BB`BB`BBbNBcTBdZBe`BjBn�Bq�Bt�Bu�Bw�B|�B~�B�%B�=B�=B�PB�bB�uB��B��B��B��B��B��B��B��B��B�B�'B�?B�^B�jB�qB��BÖBĜBƨBȴB��B��B��B��B��B��B�B�)B�BB�ZB�`B�mB�yB�B�B�B�B��B��B��B��B��B��B	B	B	B	%B	+B		7B	DB	JB	PB	PB	PB	bB	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	$�B	%�B	%�B	&�B	&�B	(�B	+B	.B	/B	0!B	49B	5?B	6FB	6FB	7LB	7LB	<jB	>wB	@�B	A�B	B�B	C�B	D�B	F�B	I�B	J�B	M�B	N�B	Q�B	R�B	R�B	S�B	VB	XB	]/B	`BB	cTB	gmB	hsB	iyB	p�B	p�B	q�B	r�B	r�B	s�B	s�B	w�B	z�B	{�B	|�B	~�B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�DB	�DB	�JB	�PB	�PB	�VB	�VB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	ŢB	�BB	�B
B
oB
�B
&�B
/B
8RB
@�B
E�B
K�B
T�B
\)B
bNB
ffB
jB
p�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�@B�=B�@B�BB�@B�6B�4B�0B�SBŜB�<B�B��BĕBȯB�vB�}B�TB�B�B��B��B��B��B�LB�GB�:B�4B�B��B��B��B��B��B��B�9B\!BG�B1B1BB�BU�B_5B\ BL�BP�BQ�BR�Ba=B^+BYBYBU�BN�BK�BO�BO�BM�BH�BE�BC�BB�BB�BA�B>rB=gB?qB=hB:UB55B2$B-B&�B"�B�B�B�BzBBBB��B��B�8BƟB�2B��B��BN�B&�B��B�DB��BďB��B�BE�B B
��B
�B
��B
��B
��B
r�B
X	B
H�B
)�B
zB	��B	�XB	��B	�IB	��B	��B	�
B	s�B	e]B	P�B	B�B	2-B	%�B	�B	�B	^B	JB	3B	)B	B	  B��B�B�VB�VB�XB�PB�:B�B��B��B��B��BȹB��B�UB�DB�*B��B��B��B��B��B��B��B��B��B��B��B�sB�[B�BB�>B�5B�+B�B�B�B�B�B�B�B� B�B�BB|�Bz�By�B|�Bz�By�Bv�Bq�Bj�Bh{BfnBfoBehBd^BaOB]7B\1B[+BZ%BXBWBV	BUBR�BN�BH�BE�BB�B>~B;mB9aB7TB4AB26B0B.B-B,B,B*�B)�B'�B$�B!�B�B�B�B�B�BnBpB�BbB~ByBXBlBeBGBEB4B
+B
*B	&B	%B BBBBBBBBBBBBBBBBBBBBBBBBB2BBBB2BB2BB4B	$B	$BB
+BfBcBdBJBDBQB�B�B�B�B�B�B�B�B�B#�B%�B(�B+	B+	B+
B+	B.B0%B1,B3;B6KB6LB8XB;lB<oB<pB=yB>|B@�BA�BC�BC�BC�BC�BD�BE�BH�BI�BK�BL�BQ�BVBYB^:B`CB`FB`DB`FB`FB`EBbRBcXBd]BefBj�Bn�Bq�Bt�Bu�Bw�B|�B~�B�'B�?B�?B�RB�gB�tB��B��B��B��B��B��B��B��B��B�B�'B�@B�]B�iB�pB��BÕBěBƥBȴB��B��B��B��B��B��B�B�%B�?B�YB�^B�kB�uB�B�B�B�B��B��B��B��B��B��B	B	B	B	!B	(B		5B	?B	EB	LB	JB	LB	_B	{B	�B	�B	�B	�B	�B	�B	 �B	#�B	$�B	%�B	%�B	&�B	&�B	(�B	*�B	.B	/B	0B	42B	58B	6@B	6?B	7EB	7EB	<dB	>oB	@|B	A�B	B�B	C�B	D�B	F�B	I�B	J�B	M�B	N�B	Q�B	R�B	R�B	S�B	U�B	XB	]'B	`:B	cLB	gdB	hjB	ipB	p�B	p�B	q�B	r�B	r�B	s�B	s�B	w�B	z�B	{�B	|�B	~�B	��B	��B	�B	�B	�B	�B	�(B	�0B	�<B	�=B	�AB	�HB	�IB	�LB	�LB	�nB	�rB	�uB	�yB	�}B	��B	��B	��B	��B	ŘB	�7B	�B
B
bB
�B
&�B
/B
8FB
@vB
E�B
K�B
T�B
\B
b>B
fXB
jsB
p�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.14 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708122016053117081220160531170812  AO  ARCAADJP                                                                    20140721230826    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230826  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230826  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170812  IP                  G�O�G�O�G�O�                