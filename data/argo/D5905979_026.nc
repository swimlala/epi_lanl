CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:58Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170858  20220204114413  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @،�l$�1   @،O��@8�$�/�c�1&�y1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�D�%qD�[�D��fD��=D�#3D�_�D���D���D� �D�Z�D���D��D��D�O\Dڐ�D��RD�fD�N�D�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBX�B_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B�\B�\B���B���B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'�D({�D(��D){�D)��D*��D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DD�DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW��DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn�DouDo��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dy��D�#4D�YHD��)D�� D� �D�]qD��HD�ֹD�gD�XRD���D��HD��D�MDڎ�D��D�)D�L{D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AԓuAԓuAԛ�Aԣ�Aԧ�Aԥ�Aԧ�Aԣ�Aԥ�Aԧ�Aԩ�AԬAԮA԰!A԰!A԰!AԮA԰!A԰!AԲ-AԲ-AԲ-Aԧ�Aԛ�AԍPA�M�A��A�bA�bNAϾwA�{A�5?A�
=A�ĜAǉ7Aơ�A�Q�A��A�33A�A��-A��A�G�A��A�z�A��PA� �A�VA���A�XA���A�S�A���A��FA��A��
A��A���A� �A��^A�I�A��DA��hA���A��9A�n�A�v�A�bNA�v�A�33A�\)A�l�A�ƨA��HA�%A��FA�A�$�A��A�ffA�n�A�K�A���A�p�A�?}A��A���A�/A���A�`BA��A�t�A�r�A��A�|�A�{A���A�S�A��9A��A��DA���A�;dA��A���A~��A~=qA}�TA}%Az=qAx��Aw�7Au��Au&�At��At�uAr��Ar(�Aq�Ap��AnȴAm?}Ak��Ajr�Af��Ad�+Ab�Aa��A_S�A\v�A[G�AYoAW��AWVAVJAS��AO�-AL�`AH��AF  ADn�AC
=AAt�A>�jA=�mA=�A=�A=oA<-A;O�A:{A97LA8n�A7�A7p�A6�`A6$�A5��A5p�A4�A3G�A1��A0��A0�!A0Q�A/�hA-t�A+��A*��A(E�A&��A%`BA$�A#�A"A �jAK�A�A�uA�Az�A��AK�AbNA��A�DA`BAt�A-A�Ar�AffA��Al�AĜA��A�DA��A
��A
�A	�-A	7LAE�A��A��AAdZA�9A�!A�+A�mA33A��AVAK�A ��A b@�1'@�E�@��@�j@�"�@��y@���@��7@��/@��@���@�@�E�@�@�X@��@�u@�1'@�K�@�V@��;@���@�S�@旍@���@�z�@�\)@��@��@���@ݙ�@�x�@�%@۶F@�S�@���@�n�@���@���@��m@���@���@ԃ@�9X@�ƨ@�@��#@ёh@�?}@�V@���@У�@�  @�@��#@�G�@��/@�9X@�
=@ɺ^@� �@�t�@��y@��@�7L@Ý�@��#@��@��u@��
@�@���@�~�@��^@��9@��u@�Q�@��;@�@���@�E�@��^@���@��`@��@�bN@��w@�S�@�o@��@��+@���@��@��T@��#@��^@�hs@��@��/@��j@�z�@�1@��@�\)@�33@�"�@��@�ff@�E�@���@��@�bN@�1@���@�33@�~�@�@��h@���@�x�@�?}@��9@���@�j@�  @�ƨ@���@��@���@�O�@�G�@���@��\@��@��@�j@�1@�@�G�@���@��/@��@���@�=q@��@��;@�K�@���@���@��R@�ff@�@���@���@��@�`B@�X@�&�@��@��@�bN@�1@���@�dZ@�K�@�33@�o@��H@��!@��\@�~�@�E�@��#@��7@�p�@�/@���@�j@�9X@�b@��@���@��w@��@��P@�\)@�C�@�ȴ@��+@�~�@�v�@�ff@�E�@��@��-@�X@��@��#@��-@���@���@��@�`B@�/@���@��j@��/@��u@�r�@�9X@�  @��w@�|�@��H@�E�@��@��T@�{@��@�@���@��h@��7@��@�`B@��@�z�@�(�@���@��P@�K�@�o@��@��y@��@��y@��!@�n�@�ff@�V@�-@�@��@��#@��-@��7@�`B@�G�@��@��`@�Ĝ@��u@�Q�@�I�@��@�  @���@��m@���@�\)@�"�@��!@�~�@�5?@��@��T@��^@���@�G�@�?}@���@�A�@|�@sX�@i�@b��@ZGE@S�@K@Cݘ@=�9@8(�@3��@-�t@'qv@"��@��@�`@tT@�@w�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AԓuAԓuAԛ�Aԣ�Aԧ�Aԥ�Aԧ�Aԣ�Aԥ�Aԧ�Aԩ�AԬAԮA԰!A԰!A԰!AԮA԰!A԰!AԲ-AԲ-AԲ-Aԧ�Aԛ�AԍPA�M�A��A�bA�bNAϾwA�{A�5?A�
=A�ĜAǉ7Aơ�A�Q�A��A�33A�A��-A��A�G�A��A�z�A��PA� �A�VA���A�XA���A�S�A���A��FA��A��
A��A���A� �A��^A�I�A��DA��hA���A��9A�n�A�v�A�bNA�v�A�33A�\)A�l�A�ƨA��HA�%A��FA�A�$�A��A�ffA�n�A�K�A���A�p�A�?}A��A���A�/A���A�`BA��A�t�A�r�A��A�|�A�{A���A�S�A��9A��A��DA���A�;dA��A���A~��A~=qA}�TA}%Az=qAx��Aw�7Au��Au&�At��At�uAr��Ar(�Aq�Ap��AnȴAm?}Ak��Ajr�Af��Ad�+Ab�Aa��A_S�A\v�A[G�AYoAW��AWVAVJAS��AO�-AL�`AH��AF  ADn�AC
=AAt�A>�jA=�mA=�A=�A=oA<-A;O�A:{A97LA8n�A7�A7p�A6�`A6$�A5��A5p�A4�A3G�A1��A0��A0�!A0Q�A/�hA-t�A+��A*��A(E�A&��A%`BA$�A#�A"A �jAK�A�A�uA�Az�A��AK�AbNA��A�DA`BAt�A-A�Ar�AffA��Al�AĜA��A�DA��A
��A
�A	�-A	7LAE�A��A��AAdZA�9A�!A�+A�mA33A��AVAK�A ��A b@�1'@�E�@��@�j@�"�@��y@���@��7@��/@��@���@�@�E�@�@�X@��@�u@�1'@�K�@�V@��;@���@�S�@旍@���@�z�@�\)@��@��@���@ݙ�@�x�@�%@۶F@�S�@���@�n�@���@���@��m@���@���@ԃ@�9X@�ƨ@�@��#@ёh@�?}@�V@���@У�@�  @�@��#@�G�@��/@�9X@�
=@ɺ^@� �@�t�@��y@��@�7L@Ý�@��#@��@��u@��
@�@���@�~�@��^@��9@��u@�Q�@��;@�@���@�E�@��^@���@��`@��@�bN@��w@�S�@�o@��@��+@���@��@��T@��#@��^@�hs@��@��/@��j@�z�@�1@��@�\)@�33@�"�@��@�ff@�E�@���@��@�bN@�1@���@�33@�~�@�@��h@���@�x�@�?}@��9@���@�j@�  @�ƨ@���@��@���@�O�@�G�@���@��\@��@��@�j@�1@�@�G�@���@��/@��@���@�=q@��@��;@�K�@���@���@��R@�ff@�@���@���@��@�`B@�X@�&�@��@��@�bN@�1@���@�dZ@�K�@�33@�o@��H@��!@��\@�~�@�E�@��#@��7@�p�@�/@���@�j@�9X@�b@��@���@��w@��@��P@�\)@�C�@�ȴ@��+@�~�@�v�@�ff@�E�@��@��-@�X@��@��#@��-@���@���@��@�`B@�/@���@��j@��/@��u@�r�@�9X@�  @��w@�|�@��H@�E�@��@��T@�{@��@�@���@��h@��7@��@�`B@��@�z�@�(�@���@��P@�K�@�o@��@��y@��@��y@��!@�n�@�ff@�V@�-@�@��@��#@��-@��7@�`B@�G�@��@��`@�Ĝ@��u@�Q�@�I�@��@�  @���@��m@���@�\)@�"�@��!@�~�@�5?@��@��T@��^@���@�G�@�?}G�O�@�A�@|�@sX�@i�@b��@ZGE@S�@K@Cݘ@=�9@8(�@3��@-�t@'qv@"��@��@�`@tT@�@w�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�HB�NB�TB�BYBr�Bp�Bp�Bx�B|�B�+B�JB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�PB�JB�JB�B�Bx�By�Bt�Bl�BZBL�BE�B>wB1'B&�B�BDBB��B�B�NB�B��B�wB�LB�FB�-B�-B�3B�B��B�PBz�BgmBYBF�B49B�B+B
��B
�B
�`B
�B
�qB
��B
v�B
XB
6FB
-B
0!B
,B
�B	��B	�B	�;B	�;B	�;B	�NB	�)B	��B	��B	��B	�B	��B	�\B	�B	n�B	[#B	M�B	B�B	7LB	%�B	�B	VB	B��B��B�B��B��B��B��B�\B�\B�1B�B�B�%B�%B�bB�VB�PB�1B�+B�1B�%B�%B�%B�%B�+B�%B�%B�+B�B~�B}�B{�B{�By�Bp�Bn�Bl�BffBffBcTBcTBaHB`BB^5B]/B\)B[#BYBW
BT�BS�BQ�BO�BL�BK�BF�BD�BD�B@�B>wB=qB=qB;dB:^B8RB7LB6FB5?B5?B33B1'B2-B/B1'B/B/B/B0!B0!B0!B/B,B-B/B33B49B49B8RB:^B:^B;dB<jB:^B<jB;dB=qB<jB<jB<jB<jB;dB;dB<jB>wB?}B>wB?}B?}B?}BA�BA�BA�BD�BG�BI�BI�BK�BM�BN�BN�BN�BP�BQ�BS�BS�BS�BW
B^5B`BBcTBhsBiyBl�Bl�Bm�Bo�Bo�Bq�Bo�Bo�Bn�Bn�Bm�BjBgmBffBdZBe`BdZBgmBjBl�Bm�Bo�Br�Bs�Bs�Bv�By�Bz�B{�B}�B�B�B�B�B�%B�JB�\B�bB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�?B�RB�FB�FB�FB�FB�XBBBÖBǮB��B��B��B��B�B�B�;B�mB�B�B�B�B�B�B�B�ZB�TB�TB�mB�B��B��B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	
=B	PB	oB	oB	�B	�B	�B	�B	#�B	%�B	'�B	)�B	+B	,B	0!B	49B	7LB	9XB	?}B	A�B	B�B	C�B	F�B	H�B	J�B	L�B	N�B	P�B	R�B	T�B	ZB	]/B	_;B	aHB	ffB	hsB	iyB	jB	jB	m�B	p�B	q�B	w�B	|�B	�B	�+B	�7B	�PB	�\B	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�3B	�9B	�?B	�LB	�LB	�RB	�XB	�dB	�qB	�qB	��B	��B	��B	��B	B	ŢB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�/B	�;B	�BB	�BB	�HB	�HB	�NB	��B	��B
B
bB
yB
 �B
(sB
2�B
9$B
?}B
F�B
K�B
P�B
UB
ZB
`�B
eFB
i*B
m�B
p�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�BمBڋB�BPFBi�Bg�Bg�BpBtB~YB�wB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B�}B�}B|RBx:Bp	BqBk�Bc�BQTBDB<�B5�B(aB$B�B�B�]B�2B��BَB�EB�B��B��B��B�qB�qB�wB�GB�B��Br*B^�BPbB=�B+�BB
�}B
�LB
�B
ܴB
�eB
��B
�*B
n&B
OoB
-�B
$pB
'�B
#jB
�B	�TB	�B	֣B	֣B	֣B	ٶB	ӑB	�HB	�+B	��B	��B	�B	��B	z�B	f	B	R�B	EFB	:B	.�B	ZB	B	�B��B�hB�CB� B�uB�B�qB�B��B��B�Bz�B|�B}�B}�B��B��B��B�B~�B�B}�B}�B}�B}�B~�B}�B}�B~�B{�Bv�BuzBsmBsmBqbBh,Bf BdB]�B]�BZ�BZ�BX�BW�BU�BT�BS�BR�BP�BN�BL�BK�BIxBGkBDYBCTB>5B<)B<)B8B6B4�B4�B2�B1�B/�B.�B-�B,�B,�B*�B(�B)�B&�B(�B&�B&�B&�B'�B'�B'�B&�B#�B$�B&�B*�B+�B+�B/�B1�B1�B2�B3�B1�B3�B2�B5B3�B3�B3�B3�B2�B2�B3�B6	B7B6	B7B7B7B9B9B9B<.B?@BALBALBCYBEdBFjBFjBFjBHvBI}BK�BK�BK�BN�BU�BW�BZ�B`Ba	BdBdBe!Bg.Bg.Bi:Bg.Bg.Bf(Bf(Be!BbB^�B]�B[�B\�B[�B^�BbBdBe"Bg/BjABkGBkGBnZBqlBrrBsxBu�Bx�By�By�B|�B}�B��B��B��B�B�B�"B�)B�AB�ZB�_B�_B�_B�eB�wB��B��B��B��B��B��B��B��B��B��B��B�B�B�#B�:B�MB�YB�kB�~B͐BѨB��B��B�B�:B�4B�4B�.B�.B�"B��B��B��B��B�B�SB�FB�5B�/B�)B�#B�B�B�B�/B�#B�B�GB�GB�ZB�lB�rB�yB��B��B��B	�B	�B		�B		�B	
B	B	5B	@B	_B	kB	xB	!�B	"�B	#�B	'�B	+�B	.�B	0�B	7B	9B	:B	;B	>.B	@:B	BGB	DSB	F_B	HkB	JwB	L�B	Q�B	T�B	V�B	X�B	]�B	_�B	`�B	bB	bB	eB	h(B	i.B	oRB	tqB	{�B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�?B	�?B	�?B	�FB	�LB	�XB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�-B	�-B	�3B	�8B	�?B	�?B	�?B	�EB	�QB	�WB	�cB	�jB	�jB	�pB	�|B	ΈB	ώB	ЕB	ћB	ӦB	ԬB	ָB	׿B	׿B	��B	��G�O�B	�KB	�CB	��B
�B
�B
ZB
�B
*DB
0�B
6�B
>B
CtB
HDB
L�B
Q{B
XrB
\�B
`�B
eXB
hB
m 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144132022020411441320220204114413  AO  ARCAADJP                                                                    20200619170858    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170858  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170858  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114413  IP                  G�O�G�O�G�O�                