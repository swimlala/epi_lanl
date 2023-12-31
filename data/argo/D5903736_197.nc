CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-01T00:01:16Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20171001000116  20190604094030  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�*/���V1   @�*0(3�X@5�bM���d���Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D��fD�;�D�t{D�ʏD��D�J�D���D�ФD���D�3�D�l�D���D� D�?�Dڍ�D��)D��3D�7
D�i�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBh�Bo�RBw�RB�RB��)B��)B��)B��)B�\B�B�B���B���B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt�Dy�RD��)D�9�D�r>D��RD��D�H�D��D��gD���D�1HD�j�D�ٚD��D�=qDڋ�D���D���D�4�D�g]D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�jA�n�A�t�A�v�A�r�A�r�A�r�A�v�A�|�A��A�~�A��A�~�A�|�A�z�A�|�A�~�A�~�A�|�A�z�A�p�A�p�A�l�A�`BA�ZA�Q�A�Q�A�Q�A�O�A�I�A�A�A�=qA߲-A��AܮA�E�A�~�A�p�A�S�A�A�A� �AҺ^A��yA�n�A���A˛�Aȥ�AžwA��
A�{A��PA�oA�ĜA��hA�C�A�{A��A��A�l�A�  A��jA�A���A�r�A�-A�
=A��mA��-A�E�A�/A��`A��wA��A��A�G�A�ĜA�S�A��A�ZA���A�`BA��mA�K�A���A�"�A���A���A��yA�M�A��FA���A��A��
A�jA��mA��^A�dZA�ȴA�ȴA�n�A�A��jA��HA�VA��A�bA��;A���A�r�A�I�A�z�A��A�E�A�A��A�v�A��HA�5?A�A���A���A���A�"�A��A���A�5?A�-A� �A~n�A|Q�Az�Ax�Av�9Au;dAt$�Aq��AmXAi�wAgAf�Ad  Act�Ab�9Aa;dA_A]
=A[�TAZA�AX�AV��AVVAU?}AP��AM�PAK�;AJ~�AI�PAHbNAF�DAE��ACAB��AA�hA@A�A?�A=/A;��A:�9A9�7A8  A7�A6�A6{A5�A3�;A3��A3XA2�+A1��A1A01'A/x�A/VA.r�A-�A+l�A*VA)S�A(^5A'�^A'33A$��A$r�A#�FA#O�A"�!A!�#Al�A|�A��A�AjA��A`BA~�A�^A/A�`A�RAM�A`BAbNAhsA��A�A�/AQ�A�A?}A��A`BA+A�AȴA9XA�^A�A&�A
ĜA
~�A
1'A	�#A	�A	oA�A��Al�A��A  A�A��A+A�A  AK�A �uA 9X@�t�@�"�@�{@�b@�&�@���@��@�h@�  @@�
=@�@�j@��T@�ƨ@�+@�@���@�A�@�\)@���@�-@��/@�1'@ޟ�@�ƨ@��@���@�\)@�v�@��@�K�@љ�@��;@�@���@���@�^5@�x�@̣�@˾w@�"�@�ff@ɡ�@�Z@�K�@Ɵ�@�V@��@ŉ7@Ĭ@�  @�;d@��@�ff@��@�/@�V@��u@��;@�
=@��@���@�O�@��@�b@�t�@�\)@���@�hs@���@��@�r�@�ƨ@��@��y@���@�^5@�@�x�@��@�r�@��w@�"�@�ȴ@�n�@���@�hs@�/@���@��P@��H@�{@�p�@��@��@�"�@�ȴ@���@�-@�J@��@��@�Ĝ@��u@�9X@�1@�ƨ@���@�\)@��@�?}@��@���@���@���@��@��@�%@�Ĝ@��u@�Q�@��
@�K�@��H@�~�@�n�@�E�@�$�@��#@�?}@�r�@�(�@��w@�\)@��@��y@���@�v�@�-@�$�@�@��@��#@�X@�/@���@��`@��j@�Z@���@�
=@���@��!@�v�@�V@�E�@�E�@�V@�@���@�hs@�/@��@��@��@���@��j@���@��9@��D@�r�@�A�@� �@���@�+@��@��y@��R@��\@�E�@��@�J@���@��@��#@���@��@�X@�X@�O�@�/@�&�@�%@���@��9@�r�@�1'@���@���@�+@�
=@�
=@�
=@�
=@��H@���@�V@�$�@���@��T@���@��^@���@�X@���@��/@���@��9@�z�@�r�@�Z@�(�@��@�1@���@�\)@�+@��y@��R@���@�E�@��@���@�x�@�O�@�/@�V@��@���@�Ĝ@�Ĝ@�8�@�#�@��r@y�t@q�@ic�@`H@X��@RkQ@L>B@DG@=��@6�@,~(@')_@#�0@4�@L0@�8@�K@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�jA�n�A�t�A�v�A�r�A�r�A�r�A�v�A�|�A��A�~�A��A�~�A�|�A�z�A�|�A�~�A�~�A�|�A�z�A�p�A�p�A�l�A�`BA�ZA�Q�A�Q�A�Q�A�O�A�I�A�A�A�=qA߲-A��AܮA�E�A�~�A�p�A�S�A�A�A� �AҺ^A��yA�n�A���A˛�Aȥ�AžwA��
A�{A��PA�oA�ĜA��hA�C�A�{A��A��A�l�A�  A��jA�A���A�r�A�-A�
=A��mA��-A�E�A�/A��`A��wA��A��A�G�A�ĜA�S�A��A�ZA���A�`BA��mA�K�A���A�"�A���A���A��yA�M�A��FA���A��A��
A�jA��mA��^A�dZA�ȴA�ȴA�n�A�A��jA��HA�VA��A�bA��;A���A�r�A�I�A�z�A��A�E�A�A��A�v�A��HA�5?A�A���A���A���A�"�A��A���A�5?A�-A� �A~n�A|Q�Az�Ax�Av�9Au;dAt$�Aq��AmXAi�wAgAf�Ad  Act�Ab�9Aa;dA_A]
=A[�TAZA�AX�AV��AVVAU?}AP��AM�PAK�;AJ~�AI�PAHbNAF�DAE��ACAB��AA�hA@A�A?�A=/A;��A:�9A9�7A8  A7�A6�A6{A5�A3�;A3��A3XA2�+A1��A1A01'A/x�A/VA.r�A-�A+l�A*VA)S�A(^5A'�^A'33A$��A$r�A#�FA#O�A"�!A!�#Al�A|�A��A�AjA��A`BA~�A�^A/A�`A�RAM�A`BAbNAhsA��A�A�/AQ�A�A?}A��A`BA+A�AȴA9XA�^A�A&�A
ĜA
~�A
1'A	�#A	�A	oA�A��Al�A��A  A�A��A+A�A  AK�A �uA 9X@�t�@�"�@�{@�b@�&�@���@��@�h@�  @@�
=@�@�j@��T@�ƨ@�+@�@���@�A�@�\)@���@�-@��/@�1'@ޟ�@�ƨ@��@���@�\)@�v�@��@�K�@љ�@��;@�@���@���@�^5@�x�@̣�@˾w@�"�@�ff@ɡ�@�Z@�K�@Ɵ�@�V@��@ŉ7@Ĭ@�  @�;d@��@�ff@��@�/@�V@��u@��;@�
=@��@���@�O�@��@�b@�t�@�\)@���@�hs@���@��@�r�@�ƨ@��@��y@���@�^5@�@�x�@��@�r�@��w@�"�@�ȴ@�n�@���@�hs@�/@���@��P@��H@�{@�p�@��@��@�"�@�ȴ@���@�-@�J@��@��@�Ĝ@��u@�9X@�1@�ƨ@���@�\)@��@�?}@��@���@���@���@��@��@�%@�Ĝ@��u@�Q�@��
@�K�@��H@�~�@�n�@�E�@�$�@��#@�?}@�r�@�(�@��w@�\)@��@��y@���@�v�@�-@�$�@�@��@��#@�X@�/@���@��`@��j@�Z@���@�
=@���@��!@�v�@�V@�E�@�E�@�V@�@���@�hs@�/@��@��@��@���@��j@���@��9@��D@�r�@�A�@� �@���@�+@��@��y@��R@��\@�E�@��@�J@���@��@��#@���@��@�X@�X@�O�@�/@�&�@�%@���@��9@�r�@�1'@���@���@�+@�
=@�
=@�
=@�
=@��H@���@�V@�$�@���@��T@���@��^@���@�X@���@��/@���@��9@�z�@�r�@�Z@�(�@��@�1@���@�\)@�+@��y@��R@���@�E�@��@���@�x�@�O�@�/@�V@��@���@�ĜG�O�@�8�@�#�@��r@y�t@q�@ic�@`H@X��@RkQ@L>B@DG@=��@6�@,~(@')_@#�0@4�@L0@�8@�K@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B  B�BO�Bn�B�B�=B�{B�B�^B��B��B��B�B�sB��BbB#�B?}BI�BVB\)B^5B^5B]/B[#BZBZBXBXB\)B`BB{�B�B�B�B�B�B�B�1B�1B�1B�+B�B�+B�JB�\B�VB�{B��B��B�{B�\B�VB�DB�7B�1B�B�+B�VB�\B�DB�B�B�B� Bz�Bp�BhsB]/BO�BJ�B>wB1'B\B��B�`B��B�'B|�BXBE�B:^B2-B&�BhB
��B
�TB
��B
�}B
�9B
��B
��B
��B
��B
��B
�7B
y�B
k�B
]/B
P�B
?}B
1'B
$�B
�B
B	�`B	��B	�}B	�9B	��B	��B	��B	��B	�bB	�%B	}�B	u�B	l�B	e`B	aHB	XB	F�B	8RB	/B	(�B	$�B	�B	�B	oB	DB	+B	B��B��B�B�sB�`B�NB�5B�)B�#B�B�
B�B��B��B��B��B��B��BȴBƨB��B�^B�?B�3B�9B�3B�-B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�\B�VB�VB�PB�DB�DB�=B�=B�=B�=B�=B�7B�1B�1B�1B�+B�+B�+B�%B�%B�B�%B�B�B�B�B�%B�B�%B�=B�DB�JB�VB�bB�\B�\B�\B�\B�PB�DB�=B�DB�=B�7B�1B�%B�B|�Bz�Bz�B{�B{�B|�B{�Bz�Bx�Bu�Bs�Br�Bs�Bu�Bu�Bv�Bw�Bz�B� B�%B�7B�JB�PB�\B�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�9B�LB�XB�XB�^B�}B��B��BÖBɺB��B��B��B��B��B��B�
B�B�#B�5B�HB�ZB�sB�B�B�B��B��B��B��B��B	B	1B	
=B	DB	JB	bB	hB	hB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	"�B	'�B	,B	,B	0!B	1'B	1'B	2-B	2-B	49B	6FB	7LB	:^B	=qB	A�B	C�B	C�B	B�B	B�B	C�B	C�B	G�B	L�B	N�B	R�B	T�B	^5B	_;B	_;B	iyB	n�B	o�B	r�B	s�B	t�B	z�B	{�B	|�B	|�B	}�B	�B	�+B	�=B	�DB	�DB	�PB	�VB	�VB	�VB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�FB	�RB	�XB	�^B	�dB	�jB	�qB	��B	B	ÖB	ĜB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�;B	�BB	�BB	�BB	�BB	�TB	�`B	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
�B
:B
�B
%`B
+�B
33B
:�B
B�B
HfB
N�B
Q�B
X_B
_�B
dtB
f�B
j�B
o�B
r-B
u%B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�B
�B
� B
� B
� B
�$B
�"B
�(B
�-B
�5B
�1B
�4B
�4B
�-B
�-B
�7B
�8B
�=B
�CB
�JB
�PB
�PB
�WB
�PB
�PB
�HB
�OB
�NB
�WB
�^B
�B
��B1BDnBc#Bu�B~�B�B��B��B�MB�RB�hBΜB��B�FB�BVB3�B>4BJ�BP�BR�BR�BQ�BO�BN�BN�BL�BL�BP�BT�Bp_Bv�Bx�By�Bx�Bw�Bw�B|�B|�B|�B{�Bx�B{�B��B��B��B��B��B��B��B��B��B�B}�B|�Bx�B{�B��B��B�By�By�Bw�Bt}Bo`BeB\�BQ�BD_B??B2�B%�B�B�ZB��BǃB��Bq�BL�B:AB.�B&�B�BB
�yB
��B
ƚB
�*B
��B
��B
��B
�wB
�gB
�;B
}�B
n�B
`6B
Q�B
E�B
45B
%�B
�B
WB	��B	� B	��B	�DB	�B	��B	��B	��B	�]B	�-B	z�B	r�B	j�B	a^B	Z3B	VB	L�B	;{B	-$B	#�B	�B	�B	�B	fB	HB	 B�B��B��B�B�oB�SB�@B�.B�B�B�B��B��B��B��B��B��B��BºB��B��B��B�jB�?B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�|B�rB�kB�[B�OB�IB�CB�EB�:B�0B�6B*B,B-B(B)B~$B}B}B}B|B|B|B{B{BzB{BzByBxByB{By	B{B*B�2B�:B�HB�RB�IB�LB�KB�LB�?B�2B-B�5B-B~%B}#B{Bu�Bq�Bo�Bo�Bp�Bp�Bq�Bp�Bo�Bm�Bj�Bh�Bg�Bh�Bj�Bj�Bk�Bl�Bo�Bt�B{B~*B�<B�CB�OB�ZB�jB�vB�|B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�(B�=B�IB�LB�KB�pB�zB�xB��B��B��B��B��B��B��B��B��B�B�B�%B�8B�HB�bB�xB�}B�B�B�B�B�B��B��B�B�-B	 *B	6B	OB	UB	TB	`B	pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	%B	&B	&B	'B	'B	)!B	+*B	,6B	/GB	2[B	6rB	8~B	8~B	7vB	7xB	8~B	8}B	<�B	A�B	C�B	G�B	I�B	SB	T!B	T"B	^]B	c~B	d�B	g�B	h�B	i�B	o�B	p�B	q�B	q�B	r�B	u�B	|B	#B	�$B	�'B	�/B	�;B	�6B	�7B	�?B	�WB	�qB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�%B	�4B	�5B	�:B	�BB	�JB	�PB	�aB	�nB	�uB	�wB	��B	��B	��B	��B	��B	��B	��B	¯B	��B	öB	ĽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	� B	�B	�B	�1B	�<B	�>B	�HB	�IB	�LB	�TB	�VB	�aB	�aB	�gB	�kB	�iB	�xB	�tB	�zG�O�B	�B	��B
B
{B
8B
 �B
(B
/jB
7�B
==B
CaB
FrB
M6B
T�B
YHB
[�B
_�B
dWB
gB
i�B
n~111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.011(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940302019060409403020190604094030  AO  ARCAADJP                                                                    20171001000116    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171001000116  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171001000116  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094030  IP                  G�O�G�O�G�O�                