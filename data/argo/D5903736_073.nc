CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:44Z AOML 3.0 creation; 2016-05-31T19:14:36Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230544  20160531121436  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               IA   AO  4051_7090_073                   2C  D   APEX                            5368                            041511                          846 @��p�C��1   @��qv���@5O\(��e)G�z�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    IA   A   A   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyY�D��D�c3D�� D���D���D�<�D�|�D�ffD� D�I�D�|�D��fD���D�6fDچfD��3D�3D�L�D�y�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�@�A�HA=G�A^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Cp�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��=C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dta�DyUD��]D�`�D���D�ʐD��]D�:�D�z�D�d)D��D�G]D�z�D��)D���D�4)Dڄ)D���D� �D�J�D�w]D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��-A��wA�A�ĜA��wA�A���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A��wA���A��A�$�A�^5A��;A�n�A�hsA�
=A��A���A�O�A�-A�bA��A��TA�ȴA��FA��A���A���A�r�A�C�A�1A�  A��A�A�(�A���A��A���A��7A�n�A�C�A�JA���A�|�A�%A��A���A���A�K�A��^A��7A�^5A� �A��
A�|�A�l�A���A��uA���A��HA�K�A��mA�Q�A�ĜA�(�A��wA�t�A��
A��A�~�A�;dA�+A�bA��A�`BA�C�A�/A�?}A��`A��A��^A�+A���A��hA�I�A�ȴA��TA��\A��TA�v�A�oA�jA�jA��A���A���A�A�G�A���A���A��A�&�A��A���A�-A�E�A��A�A~~�AzĜAw��Au�PAt�!As|�Ar1Aq�^Ap��An��Aj��Ah��Ag�hAf�Ae�AdI�Aa�A_t�A^ �A]S�A\�A[7LAY��AW��AV��AVbNAU33AS/AQ��APjAPM�AP1'APAO�FAMO�AK��AK7LAJ�AJbAIdZAIoAH��AHffAF�AE�^AD��AD~�AC&�A>��A;�hA:��A9�A7��A5�-A4�yA3�mA3�FA3l�A3�A2�A2jA1x�A-�A,�`A+�A*=qA)�-A)dZA(5?A'&�A&ȴA%�A#G�A"bNA!x�A 9XA��A��A�yA�+AE�A1A�7A�!AbA`BA��AjAp�AA�AXAS�AjA�A�7AVAAĜAA
�+A	��A��AJAhsAn�A�TA;dAn�A7LA��A �A�A �\A jA M�A =q@�|�@�@���@�M�@��@��9@�b@�t�@���@��@���@�M�@�@�v�@��@�E�@���@�7@�ƨ@���@�1'@�@���@�1@�33@ݑh@܋D@ە�@���@ؼj@� �@�|�@�o@�@�ȴ@�v�@�5?@�z�@�J@��@�  @��@Η�@�-@��T@��T@Ͳ-@�`B@���@��@��H@�~�@ɩ�@���@�Z@�  @�t�@��@�E�@ř�@ă@�;d@�@°!@��-@��j@�bN@��;@��y@���@�ff@�@��@�1@��@�+@���@��j@��w@�"�@���@��@�j@�o@�ȴ@���@�=q@�G�@��@�(�@��@�C�@���@�=q@���@��`@�Q�@�(�@�;d@���@�ȴ@���@��+@��@�/@��/@��D@��w@�dZ@��H@�ff@��T@�p�@�7L@���@���@��@��@�j@�(�@��@�@�^5@���@���@��^@���@���@�`B@��@��@��`@���@��9@���@�z�@�Z@�9X@��@��@��@��m@��@�  @�  @�1@�1@�b@��@��@��@�b@�b@�1@�1@���@�dZ@�+@�@�ȴ@���@�v�@�V@�M�@�=q@�$�@�@��@���@��-@�`B@�V@��@� �@��
@���@���@�l�@�C�@��@��H@��\@��+@��\@�v�@�=q@�@�?}@��@��9@�j@�~�@�-@��@��@��@�G�@�&�@�&�@��@���@�Ĝ@��D@�Z@�1'@���@�ƨ@���@���@�|�@�\)@�C�@�C�@�"�@��@��@���@�ȴ@�n�@��#@�`B@��@� �@��;@�|�@�l�@�K�@�33@�33@�+@�+@�+@�+@�o@���@�E�@��@��7@�X@�7L@���@�Q�@��@�t�@�l�@�l�@�l�@�\)@�33@���@���@���@��j@�b@{�F@mV@f{@^V@U�-@P�9@Hr�@D�j@?�@8bN@.ff@'\)@"M�@�+@��@ȴ@�@�u@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��-A��wA�A�ĜA��wA�A���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A��wA���A��A�$�A�^5A��;A�n�A�hsA�
=A��A���A�O�A�-A�bA��A��TA�ȴA��FA��A���A���A�r�A�C�A�1A�  A��A�A�(�A���A��A���A��7A�n�A�C�A�JA���A�|�A�%A��A���A���A�K�A��^A��7A�^5A� �A��
A�|�A�l�A���A��uA���A��HA�K�A��mA�Q�A�ĜA�(�A��wA�t�A��
A��A�~�A�;dA�+A�bA��A�`BA�C�A�/A�?}A��`A��A��^A�+A���A��hA�I�A�ȴA��TA��\A��TA�v�A�oA�jA�jA��A���A���A�A�G�A���A���A��A�&�A��A���A�-A�E�A��A�A~~�AzĜAw��Au�PAt�!As|�Ar1Aq�^Ap��An��Aj��Ah��Ag�hAf�Ae�AdI�Aa�A_t�A^ �A]S�A\�A[7LAY��AW��AV��AVbNAU33AS/AQ��APjAPM�AP1'APAO�FAMO�AK��AK7LAJ�AJbAIdZAIoAH��AHffAF�AE�^AD��AD~�AC&�A>��A;�hA:��A9�A7��A5�-A4�yA3�mA3�FA3l�A3�A2�A2jA1x�A-�A,�`A+�A*=qA)�-A)dZA(5?A'&�A&ȴA%�A#G�A"bNA!x�A 9XA��A��A�yA�+AE�A1A�7A�!AbA`BA��AjAp�AA�AXAS�AjA�A�7AVAAĜAA
�+A	��A��AJAhsAn�A�TA;dAn�A7LA��A �A�A �\A jA M�A =q@�|�@�@���@�M�@��@��9@�b@�t�@���@��@���@�M�@�@�v�@��@�E�@���@�7@�ƨ@���@�1'@�@���@�1@�33@ݑh@܋D@ە�@���@ؼj@� �@�|�@�o@�@�ȴ@�v�@�5?@�z�@�J@��@�  @��@Η�@�-@��T@��T@Ͳ-@�`B@���@��@��H@�~�@ɩ�@���@�Z@�  @�t�@��@�E�@ř�@ă@�;d@�@°!@��-@��j@�bN@��;@��y@���@�ff@�@��@�1@��@�+@���@��j@��w@�"�@���@��@�j@�o@�ȴ@���@�=q@�G�@��@�(�@��@�C�@���@�=q@���@��`@�Q�@�(�@�;d@���@�ȴ@���@��+@��@�/@��/@��D@��w@�dZ@��H@�ff@��T@�p�@�7L@���@���@��@��@�j@�(�@��@�@�^5@���@���@��^@���@���@�`B@��@��@��`@���@��9@���@�z�@�Z@�9X@��@��@��@��m@��@�  @�  @�1@�1@�b@��@��@��@�b@�b@�1@�1@���@�dZ@�+@�@�ȴ@���@�v�@�V@�M�@�=q@�$�@�@��@���@��-@�`B@�V@��@� �@��
@���@���@�l�@�C�@��@��H@��\@��+@��\@�v�@�=q@�@�?}@��@��9@�j@�~�@�-@��@��@��@�G�@�&�@�&�@��@���@�Ĝ@��D@�Z@�1'@���@�ƨ@���@���@�|�@�\)@�C�@�C�@�"�@��@��@���@�ȴ@�n�@��#@�`B@��@� �@��;@�|�@�l�@�K�@�33@�33@�+@�+@�+@�+@�o@���@�E�@��@��7@�X@�7L@���@�Q�@��@�t�@�l�@�l�@�l�@�\)@�33@���@���@���@��j@�b@{�F@mV@f{@^V@U�-@P�9@Hr�@D�j@?�@8bN@.ff@'\)@"M�@�+@��@ȴ@�@�u@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�sB�mB�fB�NB�/B�TB�`B�mB��BoB!�B%�B(�B'�B&�B&�B'�B&�B&�B&�B$�B!�B�B�B�B�B{BVBJBVB\BPBJB%BB  B��B��B1BB��B��B�B�B�B�B�mB�NB�NB�/B�B�qB��B��B��B�\B�1B� Bw�Bn�BZB?}BA�BE�BI�BJ�BO�BE�B�B��BR�B2-BVB�BǮB��B�qB�LB�B��B�BiyBQ�B/B �BB
�B
��B
�B
��B
��B
�uB
�hB
�VB
�7B
�B
�B
x�B
cTB
]/B
YB
I�B
<jB
)�B
�B
�B
hB

=B
+B
  B	�B	�5B	��B	��B	ɺB	ÖB	�XB	�B	��B	��B	��B	�\B	�=B	�B	y�B	u�B	r�B	l�B	dZB	_;B	[#B	ZB	YB	XB	S�B	K�B	E�B	C�B	A�B	>wB	;dB	9XB	8RB	5?B	/B	)�B	&�B	"�B	�B	DB��B��B��B�B�B�mB�ZB�TB�NB�BB�;B�/B�
B��BɺBĜB��B�}B�jB�XB�FB�3B�B��B��B��B��B��B��B��B��B��B�{B�oB�bB�\B�PB�JB�=B�+B�B�B~�B|�B{�Bz�Bw�Bs�Bq�Bo�Bn�Bm�Bk�BjBiyBhsBgmBffBe`Be`Be`BdZBdZBe`Be`Be`BdZBdZBcTBcTBcTBbNBbNBaHBaHB`BB_;B]/BYBYBZB]/B]/B]/B\)B]/B]/B^5BaHBbNBbNBbNBdZBe`BffBffBiyBjBl�Bl�Bl�Bm�Bm�Bl�Bp�Bx�Bz�B{�B}�B� B�B�B�B�B�B�1B�JB�VB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�jB�wB��B��BŢBɺB��B��B��B�#B�BB�TB�ZB�fB�B�B�B�B�B��B��B��B	  B	B	B	B		7B	VB	oB	uB	�B	�B	�B	�B	�B	�B	!�B	#�B	%�B	,B	/B	2-B	49B	7LB	9XB	:^B	<jB	>wB	>wB	>wB	>wB	?}B	B�B	G�B	L�B	P�B	Q�B	R�B	T�B	XB	ZB	[#B	^5B	`BB	aHB	bNB	cTB	dZB	e`B	e`B	e`B	hsB	l�B	m�B	n�B	o�B	p�B	p�B	q�B	r�B	r�B	s�B	u�B	v�B	w�B	x�B	y�B	z�B	�B	�1B	�=B	�DB	�JB	�PB	�VB	�VB	�\B	�\B	�bB	�hB	�oB	�oB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�?B	�?B	�FB	�RB	�dB	�dB	�dB	�dB	�}B	�}B	�}B	��B	ÖB	ŢB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�5B	�HB	�ZB	�mB	�mB	�mB	�sB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
1B
hB
 �B
&�B
.B
5?B
:^B
A�B
D�B
H�B
L�B
VB
\)B
_;B
dZB
hsB
l�B
o�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�rB�YB�=B�cB�pB�|B��B�B!�B%�B)B(B&�B&�B(B&�B&�B&�B$�B!�B�B�B�B�B�BdBZBfBlB`BWB4BB B��B�BBBB��B��B�B�B�B�B�}B�^B�]B�:B�B�~B��B��B��B�hB�@B�Bw�Bn�BZ)B?�BA�BE�BI�BJ�BO�BE�B�B��BR�B23B\B�BǴB��B�xB�RB�B��B�$Bi�BQ�B/%B �BB
�B
��B
�B
��B
��B
��B
�wB
�aB
�DB
�.B
�B
x�B
c`B
]9B
Y$B
I�B
<vB
*B
�B
�B
uB

MB
:B
 B	��B	�DB	�B	��B	��B	çB	�jB	�B	��B	��B	��B	�nB	�PB	� B	y�B	u�B	r�B	l�B	doB	_QB	[8B	Z4B	Y-B	X'B	TB	K�B	E�B	C�B	A�B	>�B	;}B	9mB	8gB	5TB	/4B	*B	' B	"�B	�B	\B�B�B��B��B�B�B�tB�mB�gB�[B�UB�JB�&B��B��BĸB��B��B��B�uB�bB�NB�2B�B�B��B��B��B��B��B��B��B��B��B�~B�{B�oB�hB�[B�HB�6B�%BB}B|B{Bw�Bs�Bq�Bo�Bn�Bm�Bk�Bj�Bi�Bh�Bg�Bf�Be~BeBe~BdyBdxBe�Be�BeBdwBdyBcvBctBcuBblBbmBaiBajB`bB_YB]OBY7BY6BZ<B]NB]MB]MB\HB]PB]NB^UBahBbkBbmBbmBdvBe�Bf�Bf�Bi�Bj�Bl�Bl�Bl�Bm�Bm�Bl�Bp�Bx�Bz�B|B~B� B�%B�&B�$B�#B�"B�MB�dB�tB�}B��B��B��B��B��B��B��B��B��B��B��B��B�B�/B�9B�]B��B��B��B��BŽB��B��B��B��B�>B�[B�mB�sB�}B�B�B��B��B��B��B��B�B	 B	B	)B	6B		PB	nB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	%�B	,B	//B	2CB	4PB	7bB	9nB	:uB	<�B	>�B	>�B	>�B	>�B	?�B	B�B	G�B	L�B	P�B	RB	SB	UB	X&B	Z1B	[7B	^IB	`YB	a\B	b`B	cjB	dpB	erB	eqB	ewB	h�B	l�B	m�B	n�B	o�B	p�B	p�B	q�B	r�B	r�B	s�B	u�B	v�B	w�B	x�B	y�B	z�B	�1B	�EB	�PB	�VB	�]B	�cB	�iB	�iB	�nB	�oB	�tB	�|B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�-B	�4B	�AB	�KB	�PB	�PB	�PB	�YB	�cB	�xB	�zB	�vB	�wB	��B	��B	��B	��B	èB	ųB	��B	ǿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�'B	�'B	�-B	�4B	�4B	�HB	�ZB	�iB	�}B	�~B	�|B	�B	�~B	�}B	�}B	�}B	�}B	�}B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
@B
wB
 �B
&�B
.&B
5MB
:nB
A�B
D�B
H�B
L�B
VB
\6B
_IB
dgB
hB
l�B
o�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214362016053112143620160531121436  AO  ARCAADJP                                                                    20140721230544    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230544  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230544  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121436  IP                  G�O�G�O�G�O�                