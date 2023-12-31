CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:55Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181121041155  20190604094025  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��KHv>1   @�����@4>�Q��d畁$�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C�fC�fC�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D�\D�7�D���D���D�\D�0RD�e�D���D��D�B�D��D��qD�	HD�[�Dڏ
D��HD�
D�>�D�x�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @;�@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�zC�zC�zC�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ��DK�DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df��Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt�RDy��D�D�5qD�~�D�ƹD�D�.D�c�D�ؤD��D�@RD�}qD��4D�D�Y�Dڌ�D��D��D�<{D�vgD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�oA��A��A��A�{A��A��yA�ZA�9XA� �A�{A�A��A��HA��
A���A���A���A�ƨA�ĜA�A˼jA˶FA˸RA˸RA˸RA˶FA˶FA˶FA˴9A˩�A˛�A˗�A˝�A˝�A˗�AˋDA˅A˅Aˉ7AˋDA˓uAˬA���A˛�A�ȴA��A���A���Aˣ�A�A���AʍPA��A�Aɧ�A�l�A�XA�7LA�&�A�  AȲ-A�K�A�oAǸRA�O�A���A�v�AþwA�ZA���A�1'A��^A��A��DA��HA��A��#A���A�hsA��
A��yA�A�A�oA�A�S�A��yA��A��A�z�A��A��`A�G�A���A�1'A��;A���A�?}A�`BA���A�33A��A��9A��jA��TA��#A���A��A�ZA�bNA��
A�r�A���A��A��`A�I�A�%A�33A�|�A�r�A��!A���A�1A�;dA��A�"�A�VA}|�Ay��AxQ�Aw�^Av�AtA�AnĜAmC�Al��Al �Akp�AjȴAi�
AiVAh��Ag�Ad^5Acl�Ab��Aa�7A`�uA_�A]%A\I�A[��AX��AWt�AV�!AT��ARbAP�RAO��AN�`AN$�AL��AKVAH��AG�AG+AEp�AD�9ADv�ABbNA?��A>r�A=��A< �A:r�A7�FA4�/A3�;A3+A29XA0�yA/&�A-oA+;dA)\)A'/A$E�A#�A"�uA"M�A!�A �`A�A�^A��A��A"�A�HA��AffA�hAt�A/A  A�TA&�A��A�A�A�AȴA�A�;A��AO�A�A�A��A��A�TA�uA�A
ZA��A;dA�A�RA^5A5?A��A�7A1'A��A  AO�A �A v�@�K�@��7@�|�@��H@�J@���@�&�@��@��@��
@ꗍ@�@�ȴ@�7@�O�@�@�ƨ@�ff@�j@�1@��
@�K�@�n�@�?}@��@�&�@�j@׾w@�33@��@�ff@�O�@�1'@�;d@Ѳ-@�&�@��@�Q�@�C�@θR@�V@���@ͩ�@͉7@�x�@���@�1'@�"�@�j@ǍP@���@���@Ə\@�^5@��@�hs@��@�@�{@�b@�M�@�@���@���@�b@�t�@��@�^5@���@��@��@��P@�+@���@�M�@��@���@���@���@�@��7@���@�(�@�\)@��@�ȴ@���@��@���@�`B@�V@�(�@�dZ@�v�@���@�r�@��@�|�@�@���@��\@�n�@�E�@�{@��T@�x�@��@�1'@��F@��+@�M�@�-@���@���@�O�@��@�1'@��F@�33@��@���@�bN@�(�@�b@�  @��;@��F@���@���@��@��F@��w@��F@��@���@��P@���@���@���@��P@��P@��P@���@���@��@���@��;@�ƨ@���@�|�@��@��@��@��@�|�@�|�@�l�@�l�@�K�@��@��H@��+@�-@�@��@��@���@��h@�`B@�O�@�G�@�?}@�?}@�G�@�?}@��@��@��@��@��/@���@��
@���@��w@��@��P@�K�@�C�@�+@��y@���@���@���@���@���@�v�@�-@�{@��T@���@�X@�%@��/@��9@��9@��9@��9@��@��@��@��@��;@��w@�"�@��@��@��@�I�@��@�S�@�"�@���@���@��\@�^5@�{@�@��-@�p�@�G�@��@��j@��9@��@��@�9X@�9X@�9X@�1'@�b@�ƨ@��@��@���@���@�|�@�K�@�;d@�+@�
=@��y@��@�[W@�g�@���@zTa@p�P@g��@_�Q@X2�@PɆ@J�@D�@=IR@4Q�@+iD@#x@kQ@1'@RT@�Y@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�bA�oA��A��A��A�{A��A��yA�ZA�9XA� �A�{A�A��A��HA��
A���A���A���A�ƨA�ĜA�A˼jA˶FA˸RA˸RA˸RA˶FA˶FA˶FA˴9A˩�A˛�A˗�A˝�A˝�A˗�AˋDA˅A˅Aˉ7AˋDA˓uAˬA���A˛�A�ȴA��A���A���Aˣ�A�A���AʍPA��A�Aɧ�A�l�A�XA�7LA�&�A�  AȲ-A�K�A�oAǸRA�O�A���A�v�AþwA�ZA���A�1'A��^A��A��DA��HA��A��#A���A�hsA��
A��yA�A�A�oA�A�S�A��yA��A��A�z�A��A��`A�G�A���A�1'A��;A���A�?}A�`BA���A�33A��A��9A��jA��TA��#A���A��A�ZA�bNA��
A�r�A���A��A��`A�I�A�%A�33A�|�A�r�A��!A���A�1A�;dA��A�"�A�VA}|�Ay��AxQ�Aw�^Av�AtA�AnĜAmC�Al��Al �Akp�AjȴAi�
AiVAh��Ag�Ad^5Acl�Ab��Aa�7A`�uA_�A]%A\I�A[��AX��AWt�AV�!AT��ARbAP�RAO��AN�`AN$�AL��AKVAH��AG�AG+AEp�AD�9ADv�ABbNA?��A>r�A=��A< �A:r�A7�FA4�/A3�;A3+A29XA0�yA/&�A-oA+;dA)\)A'/A$E�A#�A"�uA"M�A!�A �`A�A�^A��A��A"�A�HA��AffA�hAt�A/A  A�TA&�A��A�A�A�AȴA�A�;A��AO�A�A�A��A��A�TA�uA�A
ZA��A;dA�A�RA^5A5?A��A�7A1'A��A  AO�A �A v�@�K�@��7@�|�@��H@�J@���@�&�@��@��@��
@ꗍ@�@�ȴ@�7@�O�@�@�ƨ@�ff@�j@�1@��
@�K�@�n�@�?}@��@�&�@�j@׾w@�33@��@�ff@�O�@�1'@�;d@Ѳ-@�&�@��@�Q�@�C�@θR@�V@���@ͩ�@͉7@�x�@���@�1'@�"�@�j@ǍP@���@���@Ə\@�^5@��@�hs@��@�@�{@�b@�M�@�@���@���@�b@�t�@��@�^5@���@��@��@��P@�+@���@�M�@��@���@���@���@�@��7@���@�(�@�\)@��@�ȴ@���@��@���@�`B@�V@�(�@�dZ@�v�@���@�r�@��@�|�@�@���@��\@�n�@�E�@�{@��T@�x�@��@�1'@��F@��+@�M�@�-@���@���@�O�@��@�1'@��F@�33@��@���@�bN@�(�@�b@�  @��;@��F@���@���@��@��F@��w@��F@��@���@��P@���@���@���@��P@��P@��P@���@���@��@���@��;@�ƨ@���@�|�@��@��@��@��@�|�@�|�@�l�@�l�@�K�@��@��H@��+@�-@�@��@��@���@��h@�`B@�O�@�G�@�?}@�?}@�G�@�?}@��@��@��@��@��/@���@��
@���@��w@��@��P@�K�@�C�@�+@��y@���@���@���@���@���@�v�@�-@�{@��T@���@�X@�%@��/@��9@��9@��9@��9@��@��@��@��@��;@��w@�"�@��@��@��@�I�@��@�S�@�"�@���@���@��\@�^5@�{@�@��-@�p�@�G�@��@��j@��9@��@��@�9X@�9X@�9X@�1'@�b@�ƨ@��@��@���@���@�|�@�K�@�;d@�+@�
=@��yG�O�@�[W@�g�@���@zTa@p�P@g��@_�Q@X2�@PɆ@J�@D�@=IR@4Q�@+iD@#x@kQ@1'@RT@�Y@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBbNBbNBbNBbNBbNBbNBbNBaHBaHBbNBcTBe`Be`BffBgmBhsBiyBiyBjBk�Bk�Bl�Bn�Bo�Bo�Bo�Bo�Bp�Bp�Bq�Bq�Bs�Bu�Bv�Bx�Bx�Bx�B{�B�B�B�B�B�7B��B�B��B�9B�wB��B�B��B�5B�B  B�B#�B(�B,B)�B+B.B/B1'B5?B8RB9XB?}BJ�BO�BS�BR�BcTBr�B�hB��B�BɺB��B�qB�jB�jB�wBÖB��B�FB�9B�'B�B��B��B�=By�Bp�BjBe`B\)BJ�BA�B9XB.B'�B'�B �B�B\BB�B�;B��B�!B��B��B�BiyBO�B-B�BhBbBJB
��B
�B
�fB
�B
��B
�dB
�B
��B
{�B
`BB
T�B
O�B
G�B
1'B
\B
B
  B	��B	��B	�B	�B	�fB	�HB	�B	ƨB	�}B	�jB	�9B	�B	��B	��B	�{B	�VB	�B	x�B	r�B	hsB	ZB	Q�B	K�B	F�B	A�B	:^B	2-B	(�B	$�B	 �B	�B	�B	uB	
=B	  B��B��B�B�ZB�B��B��B��B��BŢBB��B�qB�XB�FB�XB�XB�XB�RB�RB�RB�^B�^B�^B�XB�RB��B��B��BB��B��B��B��B��BB��B��B��B��BÖBÖBBB��B��B�}B�wB�jB�XB�LB�LB�?B�FB�LB�XB�qB�wB��B��B�qB�wB�dB�wBBÖBÖBƨB��B��BɺBŢB��B�dB�RB�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�'B�FB�XB�^B�dB�}BBŢBŢBƨBƨBƨBǮBǮBȴB��B��B��B��B��B��B��B��B�B�)B�/B�TB�fB�mB�sB�yB�B�B�B�B�B��B��B��B��B��B	B		7B	uB	�B	�B	�B	�B	�B	#�B	.B	49B	5?B	6FB	9XB	;dB	;dB	<jB	>wB	?}B	@�B	@�B	C�B	D�B	D�B	E�B	F�B	F�B	F�B	G�B	G�B	G�B	H�B	K�B	M�B	P�B	S�B	T�B	T�B	VB	W
B	XB	XB	\)B	^5B	]/B	`BB	bNB	e`B	ffB	ffB	gmB	hsB	iyB	jB	m�B	o�B	p�B	v�B	}�B	�B	�B	�B	�+B	�=B	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�FB	�^B	�wB	��B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�/B	�5B	�5B	�BB	�NB	�TB	�`B	�fB	�sB	�sB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�yB	�yB	�sB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
�B
,�B
1�B
9�B
B�B
H1B
MB
Q�B
WYB
_pB
f�B
l�B
p�B
r�B
u�B
xB
{�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B\TB\RB\TB\TB\VB\TB\TB[PB[SB\XB]ZB_iB_fB`iBatBbyBcBc�Bd�Be�Be�Bf�Bh�Bi�Bi�Bi�Bi�Bj�Bj�Bk�Bk�Bm�Bo�Bp�Br�Br�Br�Bu�B|B}B~ B#B�9B��B�B��B�=B�vB��B�	B��B�7B�B��B�B�B"�B&B#�B$�B(B)B+(B/<B2LB3UB9tBD�BI�BM�BL�B]SBl�B�bB��B�BñB�B�jB�cB�eB�sB��B��B�@B�4B�%B�B��B��B�;Bs�Bj�Bd|B_]BV(BD�B;�B3\B(B!�B!�B�B�B	^B� B��B�EB��B�,B��B��B|Bc�BI�B'!B�B{B
wB[B
��B
�B
�|B
�/B
��B
�~B
�,B
��B
vB
ZZB
OB
I�B
A�B
+CB
	zB	�>B	�"B	�B	��B	��B	�B	��B	�mB	�*B	��B	��B	��B	�\B	�4B	�B	��B	��B	�}B	|4B	r�B	l�B	b�B	TKB	LB	E�B	@�B	;�B	4�B	,`B	#'B	B	�B	�B	�B	�B	iB�2B�B��B��BމB�QB�'B�B�B��B��B��B��B��B��B�|B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�XB�B�B��B��B��B��B��B��B�B�B�B�-B�2B�0B�/B�:B�:B�AB�FB�=B�DB�IB�WB�cB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�	B�B�B�!B�&B�'B�/B�0B�OB�dB�eBݎB��B�B�B�B�B��B��B��B��B�B�B�B�!B�+B�DB	pB	�B	�B	�B	�B	�B	�B	B	(LB	.oB	/wB	0~B	3�B	5�B	5�B	6�B	8�B	9�B	:�B	:�B	=�B	>�B	>�B	?�B	@�B	@�B	@�B	A�B	A�B	A�B	B�B	E�B	HB	KB	N+B	O3B	O3B	P9B	Q@B	RDB	RGB	V_B	XjB	WdB	ZwB	\�B	_�B	`�B	`�B	a�B	b�B	c�B	d�B	g�B	i�B	j�B	q B	x+B	|>B	~OB	~LB	�cB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�(B	�(B	�(B	�.B	�1B	�7B	�MB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�*B	�,B	�-B	�2B	�;B	�@B	�AB	�>B	�KB	�QB	�VB	�QB	�SB	�]B	�bB	�dB	�fB	�sB	�B	݇B	ߓB	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	�6B
!B
�B
&�B
+�B
4"B
<�B
B`B
GIB
LB
Q�B
Y�B
`�B
f�B
k$B
l�B
p%B
r0B
u�B
y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.006(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940252019060409402520190604094025  AO  ARCAADJP                                                                    20181121041155    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041155  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041155  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094025  IP                  G�O�G�O�G�O�                