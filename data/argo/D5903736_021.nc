CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:17Z AOML 3.0 creation; 2016-05-31T19:14:28Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230517  20160531121428  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_021                   2C  D   APEX                            5368                            041511                          846 @�gF�OG�1   @�gG��@3�7KƧ��d�O�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyffD�	�D�,�D���D��3D��D�P D�0 D��fD�3D�I�D��fD��fD��D�\�Dڐ D��fD���D�33D�i�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B�\B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B��HC 
=C
=C
=C
=C
=C

=C
=C
=C
=C
=C
=C
=C
=C
=C
=C
=C 
=C"
=C$
=C&
=C(
=C*
=C,
=C.
=C0
=C2
=C4
=C6
=C8
=C:
=C<
=C>
=C@
=CB
=CD
=CF
=CH
=CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX
=CZ
=C\
=C^
=C`
=Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Cp
=Cr
=Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtu�Dyh�D�
�D�.D��D��{D�D�QHD�1HD�׮D�{D�J�D���D�ǮD�D�^DڑHD�׮D���D�4{D�j�D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1A�JA�
=A�A��A؟�AؑhA�jA�O�A�A�A�9XA�33A�&�A��A�oA�A��Aץ�A֍PA�`BAԾwA�oA�\)A��A�ȴA�^5A��A�v�A�O�A�?}A��A�S�A��`A��A͡�A�-A�z�A�A���A˓uA�1A�oA��AŴ9A�A�A��;A�=qA�-A�7LA�A�A��A��hA�n�A�E�A�;dA��TA��+A�M�A�{A�1A�XA�JA��RA��A��/A�E�A��jA���A���A�E�A�  A��A��A��A��FA�\)A�K�A��jA�p�A�XA�z�A�A�(�A�r�A�JA�5?A�9XA�hsA�$�A�~�A��A��9A��A���A��HA�M�A�A�A��PA��HA��wA�t�A��DA��A�O�A�VA��A���A�M�A�VA���A���A���A�-A}G�Ay�7At �An��Aj��AhE�AfI�Ad=qAc;dA_��A]��A\E�A[��A[hsAZz�AYVAV  ATjAT�ASC�AP~�AOS�AO33ANA�AK�#AJĜAH�AC��AB�\AA?}A>�\A:�A9�A8ĜA81'A81A7�A7+A6��A4�+A3G�A2�9A1��A0A.�HA-�A,v�A+��A+t�A*��A)O�A(9XA'|�A&I�A#G�A!�FA!dZA �!A��A\)A�HA{AVAE�A�`A�AJAƨA�A�DA�A�wA~�A\)A^5AVAJA?}A�TA��A  A;dA
bNA�\Ap�Ar�A�Al�Ap�AdZA�/AA�A�9AI�A33@��;@��H@�$�@���@��@��@��#@��@�@���@��@��@��@�S�@�-@���@��@��@�5?@�5?@��@�j@��u@�z�@�  @�t�@�
=@��@܃@���@��@�+@֗�@�v�@�?}@ԛ�@�|�@щ7@�/@�V@д9@Ͼw@�dZ@�C�@��@�@���@�r�@��@��@ȼj@� �@��m@ǥ�@�\)@�
=@�=q@�&�@ċD@��y@��@�X@���@��m@���@���@���@�hs@��@�I�@�9X@�ƨ@�l�@��@���@�M�@�@���@�?}@�&�@��@�I�@�b@��
@�K�@��H@���@�{@���@��@��#@��h@�?}@�(�@��w@�t�@��@�ȴ@��#@��@���@���@�5?@��T@���@�/@��@�bN@�bN@�bN@�bN@� �@��@�K�@��@��@���@�=q@��@�O�@���@�A�@��F@�"�@�@�ȴ@���@�E�@�hs@��j@�  @�ƨ@�ƨ@��w@��@��@�"�@�o@��@���@�~�@�J@��7@��9@�Z@�(�@���@�t�@�;d@��@��R@�ff@�V@�M�@�5?@�@��h@���@��u@��@�z�@�j@�I�@� �@��;@���@�S�@�"�@��@���@��+@�~�@�ff@�@�p�@�/@��/@��j@�z�@�A�@��m@��F@�|�@�l�@�dZ@��@���@�v�@�5?@���@�&�@�%@���@��j@�bN@��;@��F@�t�@�+@��@�@��@���@��H@��R@�M�@�$�@���@���@��h@��7@��7@�&�@���@��@�Z@�bN@�Q�@�Z@�Z@�Z@�Z@�Q�@�I�@�9X@� �@���@�|�@�C�@�"�@���@��+@�n�@�V@�5?@��@�{@�J@��T@�?}@���@��D@�b@��
@��F@��P@�K�@�
=@��!@���@�~�@��#@���@���@�p�@�X@�/@�Ĝ@�Q�@��;@���@�l�@�K�@�"�@�@��y@���@���@��\@�n�@�M�@�5?@��@�`B@���@��j@��@��@�1'@�V@y��@e?}@`1'@\�@Q�^@J^5@B�@=�h@7�P@2^5@+��@'+@$I�@ A�@ƨ@;d@�\@l�@9X@	G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�1A�JA�
=A�A��A؟�AؑhA�jA�O�A�A�A�9XA�33A�&�A��A�oA�A��Aץ�A֍PA�`BAԾwA�oA�\)A��A�ȴA�^5A��A�v�A�O�A�?}A��A�S�A��`A��A͡�A�-A�z�A�A���A˓uA�1A�oA��AŴ9A�A�A��;A�=qA�-A�7LA�A�A��A��hA�n�A�E�A�;dA��TA��+A�M�A�{A�1A�XA�JA��RA��A��/A�E�A��jA���A���A�E�A�  A��A��A��A��FA�\)A�K�A��jA�p�A�XA�z�A�A�(�A�r�A�JA�5?A�9XA�hsA�$�A�~�A��A��9A��A���A��HA�M�A�A�A��PA��HA��wA�t�A��DA��A�O�A�VA��A���A�M�A�VA���A���A���A�-A}G�Ay�7At �An��Aj��AhE�AfI�Ad=qAc;dA_��A]��A\E�A[��A[hsAZz�AYVAV  ATjAT�ASC�AP~�AOS�AO33ANA�AK�#AJĜAH�AC��AB�\AA?}A>�\A:�A9�A8ĜA81'A81A7�A7+A6��A4�+A3G�A2�9A1��A0A.�HA-�A,v�A+��A+t�A*��A)O�A(9XA'|�A&I�A#G�A!�FA!dZA �!A��A\)A�HA{AVAE�A�`A�AJAƨA�A�DA�A�wA~�A\)A^5AVAJA?}A�TA��A  A;dA
bNA�\Ap�Ar�A�Al�Ap�AdZA�/AA�A�9AI�A33@��;@��H@�$�@���@��@��@��#@��@�@���@��@��@��@�S�@�-@���@��@��@�5?@�5?@��@�j@��u@�z�@�  @�t�@�
=@��@܃@���@��@�+@֗�@�v�@�?}@ԛ�@�|�@щ7@�/@�V@д9@Ͼw@�dZ@�C�@��@�@���@�r�@��@��@ȼj@� �@��m@ǥ�@�\)@�
=@�=q@�&�@ċD@��y@��@�X@���@��m@���@���@���@�hs@��@�I�@�9X@�ƨ@�l�@��@���@�M�@�@���@�?}@�&�@��@�I�@�b@��
@�K�@��H@���@�{@���@��@��#@��h@�?}@�(�@��w@�t�@��@�ȴ@��#@��@���@���@�5?@��T@���@�/@��@�bN@�bN@�bN@�bN@� �@��@�K�@��@��@���@�=q@��@�O�@���@�A�@��F@�"�@�@�ȴ@���@�E�@�hs@��j@�  @�ƨ@�ƨ@��w@��@��@�"�@�o@��@���@�~�@�J@��7@��9@�Z@�(�@���@�t�@�;d@��@��R@�ff@�V@�M�@�5?@�@��h@���@��u@��@�z�@�j@�I�@� �@��;@���@�S�@�"�@��@���@��+@�~�@�ff@�@�p�@�/@��/@��j@�z�@�A�@��m@��F@�|�@�l�@�dZ@��@���@�v�@�5?@���@�&�@�%@���@��j@�bN@��;@��F@�t�@�+@��@�@��@���@��H@��R@�M�@�$�@���@���@��h@��7@��7@�&�@���@��@�Z@�bN@�Q�@�Z@�Z@�Z@�Z@�Q�@�I�@�9X@� �@���@�|�@�C�@�"�@���@��+@�n�@�V@�5?@��@�{@�J@��T@�?}@���@��D@�b@��
@��F@��P@�K�@�
=@��!@���@�~�@��#@���@���@�p�@�X@�/@�Ĝ@�Q�@��;@���@�l�@�K�@�"�@�@��y@���@���@��\@�n�@�M�@�5?@��@�`B@���@��j@��@��G�O�@�V@y��@e?}@`1'@\�@Q�^@J^5@B�@=�h@7�P@2^5@+��@'+@$I�@ A�@ƨ@;d@�\@l�@9X@	G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB$�B$�B$�B#�B �B�B�B�B�B�B�B!�B"�B$�B&�B+B0!B7LB-B�B�B\B�B�BhBbB�BVBVBVB\BhBbB1BB��B��BB\B!�B/BH�BK�BH�BC�BA�BA�BA�BD�B@�B<jB=qB?}B>wB:^B+B(�B-B-B)�B"�B �B�B�BDBBBBoBhBDB��B�B�`B�
B��B��BĜB�'B��B��B�VB�Bz�BaHBI�B �B  B��B�B��B�?B�B��B�=B�Bz�Bl�B`BBS�B?}BVB
�B
�ZB
��B
��B
�RB
��B
�\B
v�B
m�B
N�B
2-B
�B	�B	ŢB	��B	�B	s�B	jB	e`B	aHB	W
B	K�B	@�B	C�B	T�B	YB	Q�B	P�B	J�B	G�B	A�B	7LB	49B	33B	,B	�B	�B	bB�B�B�B�/BĜBƨB��B��B��B��B��B��BɺBÖBÖB��B�qB�RB�3B�'B�B�B��B��B��B��B��B��B�bB�VB�7B�+B�%B�B�B�B}�By�Bx�Bz�By�Bz�Bz�Bz�By�Bw�Bv�Bu�Bs�Bq�Bp�Bl�BiyBffBcTB]/BW
BS�BP�BN�BM�BT�B]/B]/B[#BXBT�BS�B[#BYBYBZB[#B\)B\)B[#B[#BW
BXBW
BVBVBT�BT�BS�BR�BQ�BO�BO�BP�BP�BP�BP�BP�BP�BP�BP�BQ�BR�BS�BVBVBT�BYB]/BbNBk�Bl�Bl�Bm�Bq�Br�Bs�Bs�Bu�Bv�Bv�Bv�By�B{�B{�B|�B|�B|�B|�B}�B~�B� B�B�%B�1B�=B�DB�VB�hB�hB�hB��B��B��B��B��B��B��B��B��B�B�!B�'B�9B�^B�dB�qB�}BÖBɺB��B�B�)B�BB�mB�B��B��B��B��B��B��B��B��B	B	B	%B		7B	DB	hB	uB	�B	�B	�B	�B	�B	�B	 �B	"�B	&�B	+B	.B	2-B	7LB	:^B	?}B	E�B	F�B	H�B	I�B	L�B	T�B	\)B	aHB	cTB	dZB	e`B	e`B	ffB	jB	jB	k�B	l�B	o�B	r�B	t�B	x�B	|�B	~�B	�B	�B	�B	�%B	�=B	�PB	�VB	�VB	�VB	�\B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�FB	�LB	�XB	�^B	�jB	�qB	�}B	��B	B	B	B	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�/B	�;B	�HB	�HB	�NB	�ZB	�ZB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
JB
JB
JB
JB
JB
PB
VB
\B
\B
\B
\B
hB
hB
�B
%B
\B
&�B
5?B
9XB
<jB
C�B
G�B
L�B
S�B
ZB
_;B
e`B
iyB
m�B
r�B
v�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114114411111111111111111   B$�B$�B$�B#�B �B�B�B�B�B�B�B!�B"�B$�B&�B+B0 B7JB-B�B�B[B�B�BeB_B�BVBVBVB^BiBcB1BB��B��BB[B!�B/BH�BK�BH�BC�BA�BA�BA�BD�B@�B<hB=nB?~B>yB:]B+ B(�B-B-B)�B"�B �B�B�B@B BBBmBgBCB��B�B�_B�
B��B��BěB�$B��B��B�UB�Bz�BaFBI�B �B��B��B�B��B�=B�
B��B�8B�Bz�Bl�B`@BS�B?|BTB
�B
�YB
��B
��B
�PB
��B
�^B
v�B
m�B
N�B
21B
�B	�B	ŧB	��B	�B	s�B	j�B	ekB	aRB	WB	K�B	@�B	C�B	UB	Y B	Q�B	P�B	J�B	G�B	A�B	7UB	4DB	3=B	,B	�B	�B	nB�B�B�B�:BĭBƳB��B��B��B��B��B�B��BåBãB��B��B�bB�BB�6B�+B�B�B��B��B��B��B��B�sB�fB�IB�<B�4B�/B�$B�B~By�Bx�Bz�By�Bz�Bz�Bz�By�Bw�Bv�Bu�Bs�Bq�Bp�Bl�Bi�BfxBchB]?BWBTBP�BN�BM�BUB]BB]?B[8BX!BUBT
B[4BY(BY(BZ/B[5B\;B\>B[5B[6BWBX"BWBVBVBUBUBT	BSBQ�BO�BO�BP�BP�BP�BP�BP�BP�BP�BP�BQ�BSBTBVBVBUBY'B]ABb]Bk�Bl�Bl�Bm�Bq�Br�Bs�Bs�Bu�Bv�Bv�Bv�By�B{�B{�B|�B|�B|�B|�B~B	B�B�'B�1B�@B�KB�TB�eB�xB�wB�xB��B��B��B��B��B��B��B��B��B�!B�0B�2B�IB�jB�rB�|B��BâB��B�B�*B�4B�OB�xB�B��B��B��B��B��B�B�B�B	B	B	.B		AB	KB	pB	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	&�B	+	B	.B	26B	7QB	:fB	?�B	E�B	F�B	H�B	I�B	L�B	UB	\.B	aPB	cYB	d_B	efB	egB	fkB	j�B	j�B	k�B	l�B	o�B	r�B	t�B	x�B	|�B	~�B	�	B	�B	� B	�+B	�DB	�WB	�\B	�YB	�[B	�`B	�iB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�8B	�IB	�RB	�[B	�cB	�mB	�uB	�B	��B	B	B	B	ŧB	ǱB	ȶB	ɽB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�$B	�/B	�=B	�IB	�IB	�RB	�\B	�\B	�bB	�pB	�vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
B
B
B
B
B
!B
B
'B
'B
%B
.B
2B
	8B
	6B

>B

?B

=B
EB
GB
EB
KB
MB
IB
MB
HB
QB
XB
^B
_B
\B
_G�O�B
iB
�G�O�G�O�B
&�B
5AB
9TB
<hB
C�B
G�B
L�B
S�B
ZB
_<B
e^B
ivB
m�B
r�B
v�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114114411111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.04 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214282016053112142820160531121428  AO  ARCAADJP                                                                    20140721230517    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230517  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230517  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121428  IP                  G�O�G�O�G�O�                