CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:19Z AOML 3.0 creation; 2016-05-31T19:14:28Z UW 3.1 conversion     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20140721230519  20160531121428  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_026                   2C  D   APEX                            5368                            041511                          846 @�t�� 1   @�t�cO�@4�����d�1&�y1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DWfDW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��D�P D��fD�L�D�	�D�@ D���D�ɚD�3D�0 D��3D�ٚD�fD�33DچfD�ɚD�3D�9�D�y�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @.�R@{�@�@�A z�A>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Cj�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DSuDS��DT{�DT��DU{�DU��DV{�DW�DW��DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Dy�D��D�M�D��)D�J�D�]D�=�D��]D��]D� �D�-�D���D��]D�)D�0�Dڄ)D��]D� �D�7]D�w]D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AΣ�AΣ�AΥ�AΣ�AΡ�AΡ�AΡ�AΧ�AΥ�AΣ�AΙ�AΟ�AΛ�AΝ�AΗ�AΑhA�|�A�33A�A���A͕�A�+A��mA��A�bA�/A�Q�A�l�A͉7Aͩ�A���A��A�JA�bNAθRAΡ�AΑhA�hsA�M�A�E�A�G�A�$�A��#A�1'A��A��A��TA�  A���A�`BA��A�
=A���A���A���A���A���A�ĜA�1A��TA�\)A�O�A���A�A�bA��#A���A��A���A�7LA�r�A��#A�oA��A�ZA��uA��!A��A�|�A���A�"�A�ffA��
A�`BA��TA�t�A��9A��TA�A�A���A�G�A���A��7A�v�A�/A�bNA��DA��jA�=qA�l�A���A�r�A���A���A��7A�-A��+A��^A��A�O�A�bA~jA|�jAy\)Ax�Av-At��As7LAqoAp��Ao�PAmG�Ak�wAj�+Ai
=Ag��Ae�AdbNAa��A_�wA_C�A^n�A]?}A[��AX�AU�AT9XAR �AQ?}AO��AN9XAL-AJ^5AF��AEoADJAB��AB  A@�A>=qA<5?A:9XA8��A7/A5"�A3+A2v�A2E�A2  A/��A-��A+�A)%A'%A%dZA$$�A"=qA!&�A��AO�AAVA�wAXAȴA�AQ�A��AK�A~�AS�Az�A��A��A�A�
A�\A�TA�/A��Az�A�7A
�+A	�A�\A-A�DA�;A%A��A�A/A�Av�A1AƨAG�A bNA �@�5?@�  @��H@�`B@�33@���@�ƨ@��@���@��@�ff@���@��@�hs@�;d@�7@�Z@�~�@�/@���@��#@���@�l�@�E�@ؼj@� �@��;@֏\@�G�@���@ԃ@�l�@�ff@Ѳ-@�%@У�@�@�^5@�hs@̣�@�r�@�  @ˮ@�"�@�V@���@ɡ�@��@ȣ�@�1@Ǿw@�\)@��@Ɵ�@�=q@Ų-@�&�@ċD@��@�ƨ@�\)@��y@�5?@��^@�O�@�?}@�/@��/@�1'@��@�K�@���@��T@��h@��u@��m@�dZ@��R@�-@���@�p�@�G�@�Z@�b@��@��@�K�@���@�=q@��-@�O�@�G�@�/@��/@�bN@�1'@��;@��@�33@���@�E�@�=q@�J@���@�V@���@��@��P@�C�@��@���@��+@�ff@�@��^@�7L@�r�@���@��w@�+@��@�~�@�^5@�-@��^@��h@��7@�G�@��/@�Z@���@�E�@��@��/@���@��9@���@�bN@�Q�@�  @��@�+@�
=@�o@��H@���@�E�@�5?@��@�E�@��@���@��@�O�@��@���@���@�j@�(�@�b@���@���@��F@��@�dZ@�C�@�33@�+@�+@�@�5?@���@�?}@�%@���@�Q�@�I�@�  @��@���@�C�@��@���@�V@��@���@�@��-@�p�@�7L@�Ĝ@��u@��@�bN@�b@���@��w@��@�l�@�C�@�@��@�ȴ@�^5@�-@�J@���@��^@��h@�V@��@��u@�z�@�Z@���@�33@�
=@��@�ȴ@��!@��+@�^5@�{@��#@���@�/@�Ĝ@�j@�  @���@���@��@�dZ@�"�@�n�@�@��7@�hs@��j@�9X@��;@��@�t�@�o@�ȴ@�^5@�n�@�=q@��^@��@�O�@���@��j@��9@��@��@�(�@�b@���@��;@���@�l�@�"�@�
=@��@�ȴ@�n�@�5?@�5?@�-@�$�@��@���@�V@���@��9@���@��`@��`@��@��@�K�@}V@p��@k�F@d�/@["�@S��@K��@D�@=�@9X@3��@-V@&ff@ 1'@�@ff@�!@$�@�D@
J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AΣ�AΣ�AΥ�AΣ�AΡ�AΡ�AΡ�AΧ�AΥ�AΣ�AΙ�AΟ�AΛ�AΝ�AΗ�AΑhA�|�A�33A�A���A͕�A�+A��mA��A�bA�/A�Q�A�l�A͉7Aͩ�A���A��A�JA�bNAθRAΡ�AΑhA�hsA�M�A�E�A�G�A�$�A��#A�1'A��A��A��TA�  A���A�`BA��A�
=A���A���A���A���A���A�ĜA�1A��TA�\)A�O�A���A�A�bA��#A���A��A���A�7LA�r�A��#A�oA��A�ZA��uA��!A��A�|�A���A�"�A�ffA��
A�`BA��TA�t�A��9A��TA�A�A���A�G�A���A��7A�v�A�/A�bNA��DA��jA�=qA�l�A���A�r�A���A���A��7A�-A��+A��^A��A�O�A�bA~jA|�jAy\)Ax�Av-At��As7LAqoAp��Ao�PAmG�Ak�wAj�+Ai
=Ag��Ae�AdbNAa��A_�wA_C�A^n�A]?}A[��AX�AU�AT9XAR �AQ?}AO��AN9XAL-AJ^5AF��AEoADJAB��AB  A@�A>=qA<5?A:9XA8��A7/A5"�A3+A2v�A2E�A2  A/��A-��A+�A)%A'%A%dZA$$�A"=qA!&�A��AO�AAVA�wAXAȴA�AQ�A��AK�A~�AS�Az�A��A��A�A�
A�\A�TA�/A��Az�A�7A
�+A	�A�\A-A�DA�;A%A��A�A/A�Av�A1AƨAG�A bNA �@�5?@�  @��H@�`B@�33@���@�ƨ@��@���@��@�ff@���@��@�hs@�;d@�7@�Z@�~�@�/@���@��#@���@�l�@�E�@ؼj@� �@��;@֏\@�G�@���@ԃ@�l�@�ff@Ѳ-@�%@У�@�@�^5@�hs@̣�@�r�@�  @ˮ@�"�@�V@���@ɡ�@��@ȣ�@�1@Ǿw@�\)@��@Ɵ�@�=q@Ų-@�&�@ċD@��@�ƨ@�\)@��y@�5?@��^@�O�@�?}@�/@��/@�1'@��@�K�@���@��T@��h@��u@��m@�dZ@��R@�-@���@�p�@�G�@�Z@�b@��@��@�K�@���@�=q@��-@�O�@�G�@�/@��/@�bN@�1'@��;@��@�33@���@�E�@�=q@�J@���@�V@���@��@��P@�C�@��@���@��+@�ff@�@��^@�7L@�r�@���@��w@�+@��@�~�@�^5@�-@��^@��h@��7@�G�@��/@�Z@���@�E�@��@��/@���@��9@���@�bN@�Q�@�  @��@�+@�
=@�o@��H@���@�E�@�5?@��@�E�@��@���@��@�O�@��@���@���@�j@�(�@�b@���@���@��F@��@�dZ@�C�@�33@�+@�+@�@�5?@���@�?}@�%@���@�Q�@�I�@�  @��@���@�C�@��@���@�V@��@���@�@��-@�p�@�7L@�Ĝ@��u@��@�bN@�b@���@��w@��@�l�@�C�@�@��@�ȴ@�^5@�-@�J@���@��^@��h@�V@��@��u@�z�@�Z@���@�33@�
=@��@�ȴ@��!@��+@�^5@�{@��#@���@�/@�Ĝ@�j@�  @���@���@��@�dZ@�"�@�n�@�@��7@�hs@��j@�9X@��;@��@�t�@�o@�ȴ@�^5@�n�@�=q@��^@��@�O�@���@��j@��9@��@��@�(�@�b@���@��;@���@�l�@�"�@�
=@��@�ȴ@�n�@�5?@�5?@�-@�$�@��@���@�V@���@��9@���@��`@��`@��@��@�K�@}V@p��@k�F@d�/@["�@S��@K��@D�@=�@9X@3��@-V@&ff@ 1'@�@ff@�!@$�@�D@
J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�5B�5B�5B�5B�5B�;B�;B�;B�;B�;B�;B�;B�)B�)B�#B�B�B��B��BɺBǮBŢBǮB��B�/B�fB�B��B��B1BuB�B,BL�Bp�Bq�Br�Bw�B}�B�%B�VB�\B�=B{�BdZBL�B5?B�BuBhB\BPBDB\BuBuBoBhB\BJBDBhBoB�B�B�B{B+B�B�B�mB��BƨB�RB��B��B�=Bv�Bo�B]/B?}B0!B�BB�B�TB�B�'B��B��B�7B|�Bk�BXBB�B49B�B
��B
�;B
��B
�?B
�B
�oB
t�B
q�B
jB
VB
J�B
G�B
D�B
@�B
8RB
0!B
!�B
�B
bB
%B	��B	�B	�B	�sB	�#B	��B	��B	ɺB	��B	�?B	��B	��B	�PB	�1B	�B	v�B	l�B	ZB	J�B	B�B	7LB	1'B	)�B	&�B	!�B	�B	+B	  B��B��B��B��B�B�B�`B�HB�/B�
B��B��B��B��BǮB��B�dB�LB�-B�!B�B��B��B��B��B��B��B�{B��B�{B�hB�DB�7B�B}�By�By�Bw�Bx�Bv�Bt�Bs�Bq�Bq�Bo�Bn�Bm�Bk�BjBk�BiyBjBiyBjBjBk�Bm�Bm�Bm�Bn�Bn�Bn�Bp�Bo�Bn�Bm�Bl�Bl�Bl�Bl�Bn�Bn�Bp�Bp�Bo�Bp�Bp�Bs�Bu�Bw�Bw�By�Bz�B|�B~�B�B�B�B�+B�1B�+B�=B�JB�PB�PB�bB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�3B�?B�?B�LB�XB�dB�qB�}B��BBÖBŢBɺB��B��B��B��B��B��B��B�
B�#B�)B�HB�fB�yB�B�B��B��B��B��B��B��B��B	  B	B	%B	+B	
=B	DB	PB	bB	oB	oB	�B	�B	�B	�B	!�B	!�B	!�B	$�B	&�B	/B	1'B	1'B	33B	5?B	7LB	7LB	8RB	:^B	=qB	C�B	G�B	I�B	I�B	L�B	P�B	S�B	S�B	S�B	W
B	YB	YB	^5B	aHB	cTB	dZB	dZB	dZB	gmB	hsB	iyB	jB	k�B	m�B	m�B	r�B	t�B	v�B	y�B	z�B	~�B	�B	�B	�B	�+B	�7B	�=B	�=B	�7B	�7B	�=B	�DB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�-B	�9B	�LB	�LB	�RB	�XB	�dB	�jB	�jB	�wB	�wB	��B	��B	ÖB	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�;B	�HB	�NB	�ZB	�fB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
	7B

=B
DB
DB
JB
\B
�B
�B
$�B
'�B
2-B
7LB
=qB
@�B
F�B
J�B
P�B
XB
aHB
ffB
gmB
l�B
n�B
r�B
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�<B�<B�<B�9B�<B�AB�DB�AB�AB�DB�DB�DB�/B�/B�*B�$B� B��B��B��BǲBŤBǵB� B�8B�nB�B��B��B:B|B�B,BL�Bp�Bq�Br�Bw�B~ B�4B�_B�fB�GB{�BdgBL�B5IB�B~BoBfBWBNBbBB~BwBoBdBTBOBoBxB�B�B�B�B4B�B�B�uB��BƮB�[B��B��B�BBv�Bo�B]7B?�B0&B�BB�B�]B�B�-B��B��B�>B|�Bk�BXBB�B4?B�B
��B
�CB
��B
�HB
�B
�xB
t�B
q�B
j�B
VB
J�B
G�B
D�B
@�B
8^B
0.B
!�B
�B
rB
3B	��B	�B	�B	�B	�4B	�B	��B	��B	��B	�PB	�B	��B	�cB	�FB	�B	v�B	l�B	Z5B	J�B	B�B	7dB	1?B	*B	' B	!�B	�B	BB	 B�B�B��B��B��B�B�yB�aB�JB�%B��B�B��B��B��B��B��B�iB�IB�=B�'B��B��B��B��B��B��B��B��B��B��B�bB�TB�8B~By�By�Bw�Bx�Bv�Bt�Bs�Bq�Bq�Bo�Bn�Bm�Bk�Bj�Bk�Bi�Bj�Bi�Bj�Bj�Bk�Bm�Bm�Bm�Bn�Bn�Bn�Bp�Bo�Bn�Bm�Bl�Bl�Bl�Bl�Bn�Bn�Bp�Bp�Bo�Bp�Bp�Bs�Bu�Bw�Bw�By�Bz�B}BB�,B�2B�7B�JB�OB�HB�ZB�hB�mB�kB�B��B��B��B��B��B��B��B��B��B�B��B�B�B�B�!B�)B�/B�>B�AB�NB�YB�ZB�gB�tB�B��B��B��B©BñBżB��B��B��B��B��B��B�B�B�#B�;B�DB�bB�~B�B��B��B��B��B��B��B�B�B�B	 B	*B	>B	CB	
UB	\B	hB	zB	�B	�B	�B	�B	�B	�B	!�B	!�B	!�B	$�B	'B	/0B	1;B	1<B	3JB	5UB	7bB	7`B	8jB	:uB	=�B	C�B	G�B	I�B	I�B	L�B	P�B	TB	TB	TB	W!B	Y+B	Y+B	^KB	a\B	ciB	doB	dpB	dmB	g�B	h�B	i�B	j�B	k�B	m�B	m�B	r�B	t�B	v�B	y�B	z�B	B	�B	�B	�4B	�?B	�LB	�QB	�OB	�KB	�LB	�PB	�XB	�bB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�'B	�6B	�5B	�3B	�AB	�JB	�`B	�]B	�eB	�iB	�xB	�}B	�{B	��B	��B	��B	��B	çB	ĭB	ƷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�1B	�8B	�AB	�KB	�ZB	�^B	�iB	�wB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B
 B
 B
B
B
B
B
B
#B
#B
#B
(B
'B
(B
5B
CB
	EB

LB
TB
UB
ZB
kB
�B
�B
$�B
( B
2<B
7ZB
=B
@�B
F�B
J�B
P�B
XB
aSB
frB
g|B
l�B
n�B
r�B
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214282016053112142820160531121428  AO  ARCAADJP                                                                    20140721230519    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230519  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230519  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121428  IP                  G�O�G�O�G�O�                