CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:36Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141336  20220204114410  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؁�?%�t1   @؁���ц@6�bM���c�-1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BG��BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCw�fCz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>y�D>��D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�qD��D�QHD�ffD�߮D�+�D�]D��fD��D� D�33D��fD��RD�+�D�c�Dڛ�D��RD�{D�eqD��D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB �B'�RB/�RB7�RB?�RBGQ�BOQ�BW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�zCw�zCy�C{�C}�C�C��
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
C��C��
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
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D�D��D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<uD<��D={�D=��D>uD>�D?{�D?��D@{�D@��DA{�DA��DB{�DB�DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DM�DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]��D]��D^{�D^��D_{�D_��D`{�D`��Da{�Db�Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�DtθDy��D�gD�OD�d)D��qD�)HD�Z�D��)D���D��D�0�D��)D��D�)HD�aHDڙHD��D�
>D�c4D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A��#A��/A��;A��;A��;A��;A��HA��HA��TA��`A��HA��/A��
A��
A��#A���Aԛ�AԃA�p�A�XA�I�A�
=A�9XA�33A�jA��Aɇ+A�
=AȸRAȃA�9XAŮAś�AŸRA�33A�I�A���A���A��wA�hsA��A�XA��A�I�A��A��DA�E�A�ȴA�`BA�VA�~�A�-A�A���A�E�A��A��A��/A���A�E�A��
A�"�A�ZA�ȴA�VA�E�A��9A��PA�A�hsA�{A���A��A��A�oA��wA���A�-A���A���A�jA���A��^A�9XA�I�A�M�A�33A�%A�O�A��#A�ZA���A�bA���A��
A��A�{A��A���A�;dA�t�A�x�A\)A~I�A|�`A{dZAy��Ay&�Av�HAvA�Au33As�wAq+Ao+Ao
=An�`AnJAm+Alv�Aj�+Ag�
Adn�Acl�Ac%Aa7LA_+A]��A\��AZE�AY&�AX�RAW�AW�AWS�AVĜAV �AU�#AUhsAUATz�ASƨAS/ARI�AQl�APn�AOt�AO7LANv�AM/AK�AK7LAJ��AIhsAHI�AG�wAF�yAE�AD�9ADAC�AB��AAC�A?33A>�A<�RA<n�A;��A;+A:��A:M�A9�A8A�A7�A7hsA6ĜA6Q�A5��A4�!A4bA3�PA2�+A1dZA0I�A/��A/p�A.ȴA-�PA,�A+%A)��A(�`A(ZA'dZA&E�A%�FA%XA$$�A#XA"��A!��A!�hA ZA�PA��Az�A9XA��AhsA��A�;A�\A�hA��A�\A��A/A�/AM�A��A�AK�AA�A��A�;AO�A��A�\A1A�/A\)A
�yA��A�wAG�A��AAz�AK�A~�A{A��A $�@�
=@��!@��#@���@�o@�@�j@�"�@�E�@�G�@�I�@�+@�hs@�b@��@��#@�V@�F@��@�J@�?}@�j@�I�@�9X@�@��`@�Q�@��m@���@ݙ�@���@�r�@�9X@�\)@���@�5?@�p�@أ�@��T@�I�@Ӆ@�5?@Ѓ@��;@��#@�Q�@�(�@���@ˮ@ʟ�@ə�@��@ȓu@Ǖ�@�v�@Ų-@���@Õ�@���@�v�@���@�I�@���@�v�@��7@��`@���@�ff@��@���@�Q�@�
=@���@�I�@�J@�O�@�/@���@���@�1'@��;@�dZ@�ȴ@���@���@���@���@�G�@��/@���@�bN@�dZ@���@�%@���@�Ĝ@��@�  @���@��P@�
=@�M�@�$�@�$�@��@��h@�`B@�G�@�%@�j@�(�@��w@��y@���@�5?@�@�x�@��@� �@��P@��!@��\@�~�@�V@�=q@�@���@�bN@�\)@�@�@�%@��j@��@��u@��@�  @�l�@�l�@�\)@�C�@���@���@�X@�%@�bN@��m@��w@�|�@�\)@�;d@���@���@�=q@�{@�@��@��-@�hs@�&�@���@���@���@��D@�1@���@��@�|�@�\)@�K�@�
=@��@��H@�ȴ@��R@���@��!@���@���@��\@��\@�n�@�^5@�E�@�=q@��@��@��@���@���@�G�@��`@��9@��@��j@��j@���@��u@��@�Z@�b@��@��w@���@�|�@�33@�"�@�
=@���@��@���@��\@�ff@�n�@���@��+@�V@�$�@��@�5?@�$�@���@��T@��^@���@��7@�hs@�X@�O�@�G�@�?}@�7L@�7L@�&�@�V@���@��`@���@�Ĝ@��9@���@�r�@�1'@��@��;@��
@�i�@|��@p1'@h�@a��@XU2@P��@L[�@E�'@=@@6��@1�S@+�Q@'@"��@Q�@b@��@A�@7�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��#A��#A��/A��;A��;A��;A��;A��HA��HA��TA��`A��HA��/A��
A��
A��#A���Aԛ�AԃA�p�A�XA�I�A�
=A�9XA�33A�jA��Aɇ+A�
=AȸRAȃA�9XAŮAś�AŸRA�33A�I�A���A���A��wA�hsA��A�XA��A�I�A��A��DA�E�A�ȴA�`BA�VA�~�A�-A�A���A�E�A��A��A��/A���A�E�A��
A�"�A�ZA�ȴA�VA�E�A��9A��PA�A�hsA�{A���A��A��A�oA��wA���A�-A���A���A�jA���A��^A�9XA�I�A�M�A�33A�%A�O�A��#A�ZA���A�bA���A��
A��A�{A��A���A�;dA�t�A�x�A\)A~I�A|�`A{dZAy��Ay&�Av�HAvA�Au33As�wAq+Ao+Ao
=An�`AnJAm+Alv�Aj�+Ag�
Adn�Acl�Ac%Aa7LA_+A]��A\��AZE�AY&�AX�RAW�AW�AWS�AVĜAV �AU�#AUhsAUATz�ASƨAS/ARI�AQl�APn�AOt�AO7LANv�AM/AK�AK7LAJ��AIhsAHI�AG�wAF�yAE�AD�9ADAC�AB��AAC�A?33A>�A<�RA<n�A;��A;+A:��A:M�A9�A8A�A7�A7hsA6ĜA6Q�A5��A4�!A4bA3�PA2�+A1dZA0I�A/��A/p�A.ȴA-�PA,�A+%A)��A(�`A(ZA'dZA&E�A%�FA%XA$$�A#XA"��A!��A!�hA ZA�PA��Az�A9XA��AhsA��A�;A�\A�hA��A�\A��A/A�/AM�A��A�AK�AA�A��A�;AO�A��A�\A1A�/A\)A
�yA��A�wAG�A��AAz�AK�A~�A{A��A $�@�
=@��!@��#@���@�o@�@�j@�"�@�E�@�G�@�I�@�+@�hs@�b@��@��#@�V@�F@��@�J@�?}@�j@�I�@�9X@�@��`@�Q�@��m@���@ݙ�@���@�r�@�9X@�\)@���@�5?@�p�@أ�@��T@�I�@Ӆ@�5?@Ѓ@��;@��#@�Q�@�(�@���@ˮ@ʟ�@ə�@��@ȓu@Ǖ�@�v�@Ų-@���@Õ�@���@�v�@���@�I�@���@�v�@��7@��`@���@�ff@��@���@�Q�@�
=@���@�I�@�J@�O�@�/@���@���@�1'@��;@�dZ@�ȴ@���@���@���@���@�G�@��/@���@�bN@�dZ@���@�%@���@�Ĝ@��@�  @���@��P@�
=@�M�@�$�@�$�@��@��h@�`B@�G�@�%@�j@�(�@��w@��y@���@�5?@�@�x�@��@� �@��P@��!@��\@�~�@�V@�=q@�@���@�bN@�\)@�@�@�%@��j@��@��u@��@�  @�l�@�l�@�\)@�C�@���@���@�X@�%@�bN@��m@��w@�|�@�\)@�;d@���@���@�=q@�{@�@��@��-@�hs@�&�@���@���@���@��D@�1@���@��@�|�@�\)@�K�@�
=@��@��H@�ȴ@��R@���@��!@���@���@��\@��\@�n�@�^5@�E�@�=q@��@��@��@���@���@�G�@��`@��9@��@��j@��j@���@��u@��@�Z@�b@��@��w@���@�|�@�33@�"�@�
=@���@��@���@��\@�ff@�n�@���@��+@�V@�$�@��@�5?@�$�@���@��T@��^@���@��7@�hs@�X@�O�@�G�@�?}@�7L@�7L@�&�@�V@���@��`@���@�Ĝ@��9@���@�r�@�1'@��@��;G�O�@�i�@|��@p1'@h�@a��@XU2@P��@L[�@E�'@=@@6��@1�S@+�Q@'@"��@Q�@b@��@A�@7�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBr�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Bq�Bq�Bp�Bp�Bo�Bo�Bm�BjBjBɺBQ�BP�BW
BYB]/B^5BZBdZBp�By�B}�B|�B�B�{B��B�dB�^B�qB�wB�wB�jB��B��B��B�%Bq�BhsBYBD�BdZB� B�B{�By�Bx�Bw�Bz�Bo�BjBM�BB�B&�B�B�B�B�B�B"�BVB+BB��B�B�}B�3B��B�oB�B� Bu�BhsBVB9XB�BbB1B
��B
��B
�B
�NB
�B
ŢB
�RB
�!B
��B
��B
�bB
�B
z�B
r�B
gmB
_;B
XB
I�B
C�B
=qB
33B
'�B
�B
�B
uB
\B

=B
B	��B	�B	��B	ĜB	��B	�jB	�B	��B	��B	�oB	�+B	�+B	�B	�B	� B	|�B	z�B	x�B	x�B	u�B	t�B	o�B	m�B	iyB	dZB	_;B	XB	XB	VB	P�B	I�B	E�B	C�B	=qB	7LB	33B	/B	)�B	"�B	�B	�B	�B	bB	B	  B��B��B��B�B�B�B�B�B�B�sB�`B�TB�HB�#B�B��B��B��B��BȴBƨBƨBÖB��B�dB�LB�?B�3B�!B�B��B��B��B��B��B��B��B��B�{B�uB�hB�\B�\B�JB�=B�+B�B~�B{�Bz�By�Bv�Bu�Bt�Bs�Br�Bp�Bn�Bk�BgmBffBdZBdZBbNB^5BVBN�BF�BA�B@�B?}B>wB?}B<jB:^B7LB5?B8RB6FB8RB9XB;dB7LB6FB33B33B2-B1'B1'B0!B/B0!B1'B0!B0!B1'B0!B/B/B/B/B.B0!B1'B1'B1'B1'B33B49B49B49B5?B49B49B5?B6FB<jB<jB;dB;dB<jB=qBC�BF�BG�BG�BI�BO�BVBXBXB\)B^5B_;B_;BbNBbNBdZBdZBe`BgmBiyBk�Bl�Bn�Bo�Bo�Br�Bs�Bv�Bz�B~�B�%B�+B�7B�=B�DB�JB�\B�hB�uB��B��B��B��B��B��B��B��B��B�B�?B�RB�RB�jBBĜBƨB��B��B��B��B��B��B�B�
B�B�)B�/B�;B�NB�`B�mB�yB�B�B�B�B��B��B��B��B��B��B��B	B		7B	
=B	\B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	,B	1'B	6FB	:^B	=qB	=qB	?}B	?}B	?}B	B�B	B�B	E�B	F�B	G�B	G�B	K�B	L�B	N�B	P�B	P�B	Q�B	T�B	\)B	aHB	dZB	gmB	hsB	iyB	k�B	k�B	l�B	n�B	q�B	t�B	w�B	z�B	{�B	{�B	|�B	}�B	}�B	}�B	~�B	�B	�B	�%B	�DB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�9B	�dB	�jB	�wB	�}B	��B	ÖB	ĜB	ǮB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�XB	�B
�B
B
mB
!HB
,qB
1[B
72B
>�B
D�B
J�B
O�B
V�B
YKB
]�B
cnB
i_B
nIB
sMB
x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BiqBiqBiqBiqBiqBiqBiqBiqBiqBiqBiqBiqBiqBiqBiqBiqBhkBhkBgeBgeBf_Bf_BdRBaABaAB�vBH�BG�BM�BO�BS�BT�BP�B[BgVBp�Bt�Bs�Bz�B�+B�1B�B�B�B�%B�%B�B��B��B�jB|�Bh_B_)BO�B;UB[Bv�Bw�Br�Bp�Bo�Bn�Bq�BfUBa6BD�B9JB�BdB?BKBXBdB�BB��B��B�~B��B�CB��B�uB�9Bz�Bv�Bl�B_ABL�B0*BaB7B
�B
��B
�B
�cB
�'B
��B
�}B
�.B
��B
��B
��B
�AB
z�B
q�B
i�B
^PB
VB
N�B
@�B
:|B
4WB
*B
�B
qB
kB

_B
FB
(B	��B	��B	�rB	��B	��B	�tB	�[B	�B	��B	��B	�dB	~!B	~!B	{B	yB	v�B	s�B	q�B	o�B	o�B	l�B	k�B	f�B	d�B	`rB	[SB	V5B	O
B	O
B	L�B	G�B	@�B	<�B	:�B	4nB	.IB	*1B	&B	 �B	�B	�B	�B	�B	cB�!B�B��B��B�B�B�B�B�B�B�B�xB�eB�YB�MB�)B�B�B�B��B��B��B��B��B��B��B�mB�VB�IB�=B�+B�B�B��B��B��B��B��B��B��B��B��B�vB�jB�jB�XB�KB~:Bz!Bv
Br�Bq�Bp�Bm�Bl�Bk�Bj�Bi�Bg�Be�Bb�B^B]xB[lB[mBYaBUHBMBE�B=�B8�B7�B6�B5�B6�B3�B1uB.cB,WB/jB-^B/jB0pB2|B.dB-^B*LB*LB)FB(@B(@B':B&5B';B(AB';B';B(AB';B&5B&5B&5B&5B%.B';B(AB(AB(AB(AB*MB+SB+SB+SB,YB+SB+SB,YB-`B3�B3�B2~B2B3�B4�B:�B=�B>�B>�B@�BF�BMBO)BO)BSBBUNBVTBVTBYgBYgB[sB[sB\yB^�B`�Bb�Bc�Be�Bf�Bf�Bi�Bj�Bm�Bq�BvB}=B~CB�OB�UB�\B�bB�tB�B��B��B��B��B��B��B��B��B��B��B�*B�TB�gB�gB�B��B��B��B��B��B��B��B� B�B�B�B�#B�<B�BB�NB�aB�sB�B��B�B�B�B��B��B��B��B��B��B��B�B�#B	 HB	NB	lB	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#B	(5B	-TB	1lB	4B	4B	6�B	6�B	6�B	9�B	9�B	<�B	=�B	>�B	>�B	B�B	C�B	E�B	G�B	G�B	H�B	L
B	S5B	XTB	[eB	^xB	_~B	`�B	b�B	b�B	c�B	e�B	h�B	k�B	n�B	q�B	r�B	r�B	s�B	t�B	t�B	t�B	vB	yB	{"B	}.B	�MB	�eB	�jB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�-B	�9B	�?B	�jB	�pB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�>G�O�B	�[B	�B	��B
B
nB
IB
#qB
([B
.2B
5�B
;�B
A�B
F�B
M�B
PJB
T�B
ZmB
`]B
eGB
jKB
o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144102022020411441020220204114410  AO  ARCAADJP                                                                    20200618141336    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141336  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141336  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114410  IP                  G�O�G�O�G�O�                