CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:07Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170907  20220204114416  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               8A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؤVH�1   @ؤ��pr@6h1&�x��c���`A�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    8A   B   B   @���@���A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�3D�%qD�T)D���D��fD�\D�eD��=D��\D�#�D�`�D���D��)D�
D�_�DڞfD��{D��D�R�D�)D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@��\@�A�HA>�HA`z�A~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBWQ�B_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�zC�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��DuD��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!��D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt�RDy��D�#4D�Q�D���D��)D�D�b�D�� D��D�!�D�^gD���D���D��D�]qDڜ)D��>D��D�PRD��D�ׯ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�oA�oA�{A��A��A��A��A��A��A��A� �A� �A�"�A�$�A�&�A�+A�+A�/A�33A�(�A�1'A�1'A�(�A�+A�$�A�"�A�(�A�(�A�(�A�A�A�ffA��A���A��PA�jA�I�A�5?A� �A�  A��A���A��!A��uA��A�E�A��A���A�ĜA��wA��A�ƨA�I�A���A�`BA���A��;A���A�bA� �A�5?A�oA��A���A���A�p�A��yA�VA���A��A�VA��DA�A��A��!A��DA�hsA�5?A�K�A��A� �A��\A�5?A��mA�t�A���A���A�~�A�E�A��yA�x�A�^5A�=qA��jA�I�A��A��A��A��
A���A�ƨA�ȴA���A��9A���A��!A�ĜA�x�A�S�A��jA���A��mA���A���A��A��!A�z�A��RA�z�A���A�A�A�A��A}�AzA�Aw��Atn�As�hAr��ArVAq��AqXAp�Ao�-AnȴAmK�AkS�AjE�Ai�#Ai�7Ai�Ah�HAe��Aa�A`E�A^bNAZM�AX�AV^5AS�wAR(�AP�+AO"�AN�9AM��AL�ALJAH{AFA�AD��AD�AC��AC�AB�`AB�AA�;AA&�A@(�A?dZA>$�A<jA:�9A7�7A6E�A5��A5XA3K�A2bNA1�A/K�A.v�A-��A-"�A,ZA*�DA)?}A(��A(�\A'ƨA'G�A&�9A&-A%x�A$z�A"�A!/A�yA�\A��Ar�AJA`BAv�AdZAZA1Al�AJA�7A��A �AO�A��Az�A�TA`BA�A
(�A	�A��A-A�A�
AȴAQ�A&�A b@�7L@���@�n�@�@��
@�dZ@�ȴ@�p�@�ƨ@�ff@�Z@��
@��@�33@�n�@�9@��H@�J@�%@�
=@�/@�(�@�v�@ܣ�@۝�@�n�@���@�X@���@ԋD@��@ӍP@��@�Z@��@��@��/@̋D@�S�@ɑh@ȋD@�S�@ư!@�$�@� �@öF@�33@�@°!@���@��@��\@��@�x�@�&�@��@���@�Z@���@�+@�J@�?}@� �@��F@�|�@�\)@��+@�/@�|�@�^5@���@��@�X@��/@�9X@��m@��R@�E�@���@��@��T@��#@��-@��^@��^@���@���@��h@��7@�x�@�hs@�hs@�hs@�?}@� �@�S�@��H@��R@���@���@�ff@���@�p�@�G�@�7L@�&�@��/@��j@���@�Q�@�ƨ@��P@�|�@�ff@��^@�hs@��`@�9X@���@�33@��H@���@���@��\@�ff@�M�@�=q@���@��^@���@���@��7@�V@��9@�1'@���@���@�|�@�;d@���@���@�^5@�E�@��@���@���@�`B@��@��@���@���@��j@���@�z�@�bN@�I�@��w@��F@���@�t�@�C�@�o@�@��R@�ff@�V@�E�@�V@���@��\@�n�@�V@�{@��T@���@�V@��j@�r�@�1'@��@��F@���@��@�K�@��y@��R@�ff@�$�@��#@��7@���@���@��`@���@��@�I�@�ƨ@�"�@�v�@�E�@�$�@��@��h@�p�@�G�@���@��j@�r�@�1'@��;@��@���@��@�"�@��@�M�@�V@�^5@�-@��@�@�p�@�V@��`@��`@��/@�Ĝ@��@��@�Q�@�1@�\)@�@��H@��@��R@���@��!@�5?@�X@���@�r�@��;@��@���@��@�l�@�C�@�33@�o@��@��H@���@���@��R@���@��\@��!@��+@��@�V@��/@���@�oi@vYK@kZ�@dbN@\�@SH�@Kƨ@CJ#@?�@8~@-zx@*��@%4@"�@	l@4@�M@�h@X@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�bA�oA�oA�{A��A��A��A��A��A��A��A� �A� �A�"�A�$�A�&�A�+A�+A�/A�33A�(�A�1'A�1'A�(�A�+A�$�A�"�A�(�A�(�A�(�A�A�A�ffA��A���A��PA�jA�I�A�5?A� �A�  A��A���A��!A��uA��A�E�A��A���A�ĜA��wA��A�ƨA�I�A���A�`BA���A��;A���A�bA� �A�5?A�oA��A���A���A�p�A��yA�VA���A��A�VA��DA�A��A��!A��DA�hsA�5?A�K�A��A� �A��\A�5?A��mA�t�A���A���A�~�A�E�A��yA�x�A�^5A�=qA��jA�I�A��A��A��A��
A���A�ƨA�ȴA���A��9A���A��!A�ĜA�x�A�S�A��jA���A��mA���A���A��A��!A�z�A��RA�z�A���A�A�A�A��A}�AzA�Aw��Atn�As�hAr��ArVAq��AqXAp�Ao�-AnȴAmK�AkS�AjE�Ai�#Ai�7Ai�Ah�HAe��Aa�A`E�A^bNAZM�AX�AV^5AS�wAR(�AP�+AO"�AN�9AM��AL�ALJAH{AFA�AD��AD�AC��AC�AB�`AB�AA�;AA&�A@(�A?dZA>$�A<jA:�9A7�7A6E�A5��A5XA3K�A2bNA1�A/K�A.v�A-��A-"�A,ZA*�DA)?}A(��A(�\A'ƨA'G�A&�9A&-A%x�A$z�A"�A!/A�yA�\A��Ar�AJA`BAv�AdZAZA1Al�AJA�7A��A �AO�A��Az�A�TA`BA�A
(�A	�A��A-A�A�
AȴAQ�A&�A b@�7L@���@�n�@�@��
@�dZ@�ȴ@�p�@�ƨ@�ff@�Z@��
@��@�33@�n�@�9@��H@�J@�%@�
=@�/@�(�@�v�@ܣ�@۝�@�n�@���@�X@���@ԋD@��@ӍP@��@�Z@��@��@��/@̋D@�S�@ɑh@ȋD@�S�@ư!@�$�@� �@öF@�33@�@°!@���@��@��\@��@�x�@�&�@��@���@�Z@���@�+@�J@�?}@� �@��F@�|�@�\)@��+@�/@�|�@�^5@���@��@�X@��/@�9X@��m@��R@�E�@���@��@��T@��#@��-@��^@��^@���@���@��h@��7@�x�@�hs@�hs@�hs@�?}@� �@�S�@��H@��R@���@���@�ff@���@�p�@�G�@�7L@�&�@��/@��j@���@�Q�@�ƨ@��P@�|�@�ff@��^@�hs@��`@�9X@���@�33@��H@���@���@��\@�ff@�M�@�=q@���@��^@���@���@��7@�V@��9@�1'@���@���@�|�@�;d@���@���@�^5@�E�@��@���@���@�`B@��@��@���@���@��j@���@�z�@�bN@�I�@��w@��F@���@�t�@�C�@�o@�@��R@�ff@�V@�E�@�V@���@��\@�n�@�V@�{@��T@���@�V@��j@�r�@�1'@��@��F@���@��@�K�@��y@��R@�ff@�$�@��#@��7@���@���@��`@���@��@�I�@�ƨ@�"�@�v�@�E�@�$�@��@��h@�p�@�G�@���@��j@�r�@�1'@��;@��@���@��@�"�@��@�M�@�V@�^5@�-@��@�@�p�@�V@��`@��`@��/@�Ĝ@��@��@�Q�@�1@�\)@�@��H@��@��R@���@��!@�5?@�X@���@�r�@��;@��@���@��@�l�@�C�@�33@�o@��@��H@���@���@��R@���@��\@��!@��+@��@�VG�O�@���@�oi@vYK@kZ�@dbN@\�@SH�@Kƨ@CJ#@?�@8~@-zx@*��@%4@"�@	l@4@�M@�h@X@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBffBffBgmBgmBgmBgmBhsBgmBgmBgmBgmBgmBgmBhsBgmBhsBiyBiyBiyBjBhsBiyBjBhsBhsBgmBgmBgmBiyBjB�B�9B�fBuB7LBJ�BYBaHBm�Bu�By�B~�B�B�%B�1B�JB�7B�7B�7B�7B�=B�+B~�Bp�B^5B^5Bp�Bl�BffBcTBbNB_;BQ�B]/BVBR�B_;B[#B`BB^5B]/BcTB]/BW
B@�B<jB9XB6FB0!B%�B�B�BuBhB\BDBPB��B�B�NB�B�B��B��BƨB�jB�3B��B��B��B�Bn�BJ�B6FB0!B�BbBVBB
��B
�BB
��B
�9B
��B
�hB
�JB
�1B
� B
o�B
dZB
^5B
ZB
Q�B
<jB
%�B
�B	��B	�B	�B	�fB	�TB	�;B	�B	��B	��B	ĜB	�FB	�B	��B	��B	��B	��B	�oB	o�B	ffB	T�B	C�B	-B	#�B	uB	1B��B��B��B�B�B�B�`B�`B�#B�
B�B��B��B��B��B��BƨBÖB�jB�LB�B��B��B��B�{B�{B�PB�JB�B|�By�Bv�Bu�Bt�Br�Bq�Bq�Br�Bo�Bo�Bl�Bk�BffB]/BS�BN�BJ�BD�BD�BC�BB�B?}BA�B<jB;dB;dB;dB:^B9XB9XB9XB7LB7LB6FB5?B5?B5?B33B2-B2-B1'B/B+B)�B$�B#�B"�B!�B$�B!�B!�B �B �B �B �B!�B!�B!�B �B!�B �B"�B"�B!�B"�B#�B#�B#�B$�B&�B&�B'�B.B2-B2-B2-B49B49B5?B;dB=qB?}B@�B@�BC�BI�BK�BP�BQ�BT�B]/B_;BaHBbNBcTBhsBs�B{�B~�B�B�B�B�B�+B�1B�DB�PB�\B��B��B��B��B��B��B��B�B�!B�!B�!B�'B�9B�LBȴB��B��B��B��B��B�B��B�B�B�B�B�
B�B�B�B�B�
B�#B�;B�TB�yB�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	B	1B	
=B	DB	JB	\B	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	,B	1'B	33B	5?B	8RB	;dB	?}B	B�B	D�B	H�B	I�B	K�B	M�B	R�B	R�B	T�B	YB	ZB	\)B	_;B	`BB	aHB	ffB	gmB	jB	l�B	n�B	o�B	p�B	w�B	{�B	|�B	}�B	�B	�=B	�JB	�PB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�3B	�9B	�9B	�?B	�LB	�^B	�qB	��B	B	ÖB	ĜB	ĜB	ÖB	ÖB	ĜB	ĜB	ƨB	ŢB	ĜB	ĜB	ĜB	ĜB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�/B	�;B	�NB	�`B	�ZB	�TB	�NB	�
B	�+B
�B
�B
IB
%�B
/�B
6�B
=�B
A�B
J�B
T�B
W?B
]B
`�B
e�B
hsB
l"B
p!B
t�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B^KB^KB_RB_RB_RB_RB`XB_RB_RB_RB_RB_RB_RB`XB_RB`XBa^Ba^Ba^BbdB`XBa^BbdB`XB`XB_RB_RB_RBa^BbdB}B�B�CBPB/$BB�BP�BYBefBm�Bq�Bv�By�B}�B�B�B�B�B�B�B�B~�Bv�BhzBVBVBhzBdaB^=B[+BZ%BWBI�BUBM�BJ�BWBR�BXBVBUB[-BU	BN�B8_B4FB14B.#B'�B�B�BfBTB	GB<B$B0B��B�hB�1B��B��B��BìB��B�PB�B��B��B��Bz�Bf�BB�B.6B(B�BUBIB
�B
�B
�8B
��B
�2B
��B
�dB
�FB
�-B
w�B
g�B
\YB
V5B
RB
I�B
4lB
�B
�B	��B	�B	�B	�nB	�]B	�DB	� B	��B	��B	��B	�RB	�B	��B	��B	��B	��B	�~B	g�B	^xB	MB	;�B	%$B	�B	�B	 JB�B��B��B��B�B�B�|B�|B�@B�'B�!B�B�B�
B��B��B��B��B��B�lB�5B��B��B��B��B��B�sB�nBz0BuBr Bn�Bm�Bl�Bj�Bi�Bi�Bj�Bg�Bg�Bd�Bc�B^�BUWBL!BGBB�B<�B<�B;�B:�B7�B9�B4�B3�B3�B3�B2�B1�B1�B1�B/yB/yB.sB-lB-lB-lB+`B*[B*[B)UB'IB#0B"+BBBB�BB�B�B�B�B�B�B�B�B�B�B�B�BBB�BBBBBBBB !B&EB*^B*^B*^B,jB,jB-pB3�B5�B7�B8�B8�B;�BA�BC�BIBJBM.BU_BWjBYwBZ}B[�B`�Bk�BtBw(By4Bz:Bz:B}MBYB�_B�qB�}B��B��B��B��B��B��B��B�(B�@B�MB�MB�MB�SB�eB�wB��B�	B�"B�(B�(B�(B�.B�(B�.B�.B�.B�.B�4B�:B�:B�:B�:B�4B�MB�dB�}B�B�B��B��B��B��B��B�B�	B�B�B�B�"B�(B�.B�4B	 XB	dB	kB	qB	�B		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B		B	$-B	)LB	+XB	-dB	0wB	3�B	7�B	:�B	<�B	@�B	A�B	C�B	E�B	KB	KB	M!B	Q:B	R@B	TLB	W^B	XeB	YkB	^�B	_�B	b�B	d�B	f�B	g�B	h�B	o�B	tB	uB	vB	{3B	�]B	�jB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�,B	�9B	�?B	�WB	�QB	�WB	�WB	�]B	�jB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�&B	�!B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�4B	�4B	�:B	�:B	�@B	�@B	�@B	�@B	�FB	�FB	�FB	�FB	�KB	�WB	�jB	�|B	�vB	�pG�O�B	�&B	�FB	�B
�B
cB
�B
'�B
.�B
5�B
:B
B�B
L�B
OWB
U-B
YB
]�B
`�B
d:B
h9B
l�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144162022020411441620220204114416  AO  ARCAADJP                                                                    20200619170907    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170907  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170907  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114416  IP                  G�O�G�O�G�O�                