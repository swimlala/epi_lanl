CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-05T20:16:16Z AOML 3.0 creation; 2016-05-31T19:14:48Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7$   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7(   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7,   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7<   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7L   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7\   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7d   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8    DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8@   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8h   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
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
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20160305201616  20190604094000  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_141                   2C  D   APEX                            5368                            041511                          846 @ך�6��1   @ך�� G�@4�r� Ĝ�dU����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy��D���D�0 D�i�D���D�	�D�FfD���D��3D� D�0 D��3D��3D�fD��3Dڌ�D�� D�	�D�L�D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @5�@{�@�@�AG�A>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C �C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
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
C��C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt[�Dy�RD���D�-�D�g]D�ʐD�]D�D)D��]D���D��D�-�D���D���D�)D���Dڊ�D��D�]D�J�D�)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aȡ�Aȟ�Aȧ�Aȥ�Aȥ�Aȣ�Aȡ�Aȟ�Aȗ�A�~�A�VA�-A�$�A�VA��HA�ƨA�ƨA���AǸRAǴ9Aǲ-AǮAǮAǩ�AǛ�AǕ�Aǉ7AǇ+AǃAǅAǅAǅAǅAǅAǃAǋDA�z�A�`BA���A�dZA�  Aũ�A�ffA���A×�A¬A��A��hA��jA��!A�ffA��jA�1'A��\A��yA�E�A��HA�VA�?}A��A���A���A�ȴA��DA�bNA�;dA�VA��A���A��A�"�A��FA��mA���A��#A��A���A��A��;A��DA���A��A��hA�+A��/A��A�ffA��A���A��mA��A���A�^5A�oA�A�A��A�5?A��uA�VA���A���A�+A���A��A�ƨA�z�A��;A�~�A�~�A��A�#A}S�A|z�A|^5A|$�A{�Ay&�Aw"�Au7LAq��Ap�AoO�An�9Al��Ak
=Ai��AhĜAgVAe%Ad1Ab�9A`�9A^�+A[��AY&�AWƨAW7LAV�ATA�AP(�AN��AM�FALr�AK�AI�wAD��AB�yAA�mA@ĜA@I�A?�mA?�^A?/A=�A="�A9p�A8(�A5�
A4JA2�uA2JA0�RA/��A.�\A-�;A,�uA+�PA*��A)A)C�A(ffA'/A%�hA$�uA#"�A!�PA ��A`BAVAdZA��AffA�A�FA?}A��A�A�7A��A�7A��A��A�A��A��A�-At�AjA`BA�jA�mA+A1'Al�AA
�/A
 �A	
=AAoA�!AbNA�RA�jAE�A�^A�7AS�A ��A r�@�ƨ@�v�@�  @�5?@��@��7@���@��@�|�@��H@�9@�ff@��-@�(�@��@�x�@���@�n�@�?}@���@��H@�@�`B@�{@�?}@�1'@���@���@�I�@ҟ�@�{@�&�@υ@���@��y@�V@�(�@�l�@�33@��@�V@� �@Ǿw@ǶF@Ǿw@Ǿw@�ƨ@�l�@�ff@ũ�@��@�z�@�1@�l�@�~�@���@�Q�@���@�|�@�C�@���@���@�O�@��9@�Z@�1@��w@�dZ@��@���@��\@��+@�~�@�n�@�^5@�$�@��7@�7L@��@���@���@�j@�9X@���@���@���@�{@��^@���@��@�p�@��@��@� �@��F@�dZ@�33@��y@�M�@��@�X@��@��j@�1@�o@�5?@��j@�z�@���@��@��@���@��9@�z�@�(�@� �@��@��@�C�@��@���@�V@��T@���@�x�@�/@��@���@� �@���@��!@���@���@�G�@���@���@��^@��7@�&�@��`@� �@���@���@��;@��
@��w@�\)@���@��7@���@��/@��@�I�@��@��\@��T@�X@�z�@��;@��F@���@��@�K�@�"�@���@�~�@��P@�1@�l�@�ff@�V@��R@��y@�"�@��@�ƨ@��@�l�@�n�@���@���@���@�"�@�
=@��H@���@���@��#@���@��@�5?@��\@��H@���@�~�@�^5@�5?@��^@�O�@���@��w@��P@�C�@��y@���@�v�@�5?@��#@�p�@�/@�V@�%@��@�j@�Q�@�b@��@��;@��
@�t�@�C�@�"�@�"�@��H@���@�ȴ@�V@��T@��7@�x�@�x�@�p�@�hs@�G�@�7L@��@���@��j@���@��D@��@�Q�@�A�@�1'@�b@��P@��y@�=q@���@���@�v�@�$�@��@��-@�`B@��@��@���@���@���@�Q�@��
@���@�;d@�ȴ@�ff@�^5@��h@���@�$�@{��@r�!@g�w@]@UO�@Lj@E�@>$�@8��@2��@,Z@'��@"�\@V@=q@��@b@ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   Aȡ�Aȟ�Aȧ�Aȥ�Aȥ�Aȣ�Aȡ�Aȟ�Aȗ�A�~�A�VA�-A�$�A�VA��HA�ƨA�ƨA���AǸRAǴ9Aǲ-AǮAǮAǩ�AǛ�AǕ�Aǉ7AǇ+AǃAǅAǅAǅAǅAǅAǃAǋDA�z�A�`BA���A�dZA�  Aũ�A�ffA���A×�A¬A��A��hA��jA��!A�ffA��jA�1'A��\A��yA�E�A��HA�VA�?}A��A���A���A�ȴA��DA�bNA�;dA�VA��A���A��A�"�A��FA��mA���A��#A��A���A��A��;A��DA���A��A��hA�+A��/A��A�ffA��A���A��mA��A���A�^5A�oA�A�A��A�5?A��uA�VA���A���A�+A���A��A�ƨA�z�A��;A�~�A�~�A��A�#A}S�A|z�A|^5A|$�A{�Ay&�Aw"�Au7LAq��Ap�AoO�An�9Al��Ak
=Ai��AhĜAgVAe%Ad1Ab�9A`�9A^�+A[��AY&�AWƨAW7LAV�ATA�AP(�AN��AM�FALr�AK�AI�wAD��AB�yAA�mA@ĜA@I�A?�mA?�^A?/A=�A="�A9p�A8(�A5�
A4JA2�uA2JA0�RA/��A.�\A-�;A,�uA+�PA*��A)A)C�A(ffA'/A%�hA$�uA#"�A!�PA ��A`BAVAdZA��AffA�A�FA?}A��A�A�7A��A�7A��A��A�A��A��A�-At�AjA`BA�jA�mA+A1'Al�AA
�/A
 �A	
=AAoA�!AbNA�RA�jAE�A�^A�7AS�A ��A r�@�ƨ@�v�@�  @�5?@��@��7@���@��@�|�@��H@�9@�ff@��-@�(�@��@�x�@���@�n�@�?}@���@��H@�@�`B@�{@�?}@�1'@���@���@�I�@ҟ�@�{@�&�@υ@���@��y@�V@�(�@�l�@�33@��@�V@� �@Ǿw@ǶF@Ǿw@Ǿw@�ƨ@�l�@�ff@ũ�@��@�z�@�1@�l�@�~�@���@�Q�@���@�|�@�C�@���@���@�O�@��9@�Z@�1@��w@�dZ@��@���@��\@��+@�~�@�n�@�^5@�$�@��7@�7L@��@���@���@�j@�9X@���@���@���@�{@��^@���@��@�p�@��@��@� �@��F@�dZ@�33@��y@�M�@��@�X@��@��j@�1@�o@�5?@��j@�z�@���@��@��@���@��9@�z�@�(�@� �@��@��@�C�@��@���@�V@��T@���@�x�@�/@��@���@� �@���@��!@���@���@�G�@���@���@��^@��7@�&�@��`@� �@���@���@��;@��
@��w@�\)@���@��7@���@��/@��@�I�@��@��\@��T@�X@�z�@��;@��F@���@��@�K�@�"�@���@�~�@��P@�1@�l�@�ff@�V@��R@��y@�"�@��@�ƨ@��@�l�@�n�@���@���@���@�"�@�
=@��H@���@���@��#@���@��@�5?@��\@��H@���@�~�@�^5@�5?@��^@�O�@���@��w@��P@�C�@��y@���@�v�@�5?@��#@�p�@�/@�V@�%@��@�j@�Q�@�b@��@��;@��
@�t�@�C�@�"�@�"�@��H@���@�ȴ@�V@��T@��7@�x�@�x�@�p�@�hs@�G�@�7L@��@���@��j@���@��D@��@�Q�@�A�@�1'@�b@��P@��y@�=q@���@���@�v�@�$�@��@��-@�`B@��@��@���@���@���@�Q�@��
@���@�;d@�ȴ@�ff@�^5G�O�@���@�$�@{��@r�!@g�w@]@UO�@Lj@E�@>$�@8��@2��@,Z@'��@"�\@V@=q@��@b@ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�sB�sB�sB�sB�B�B�B�B�B�B��B:^BjB��B�!B�qBǮB�B��B  BuB-B33B+B-B2-B2-B1'B1'B0!B)�B�B�BJBDB1BBBB��B��B�B�fB�5B��B��B�}B�FB�!B��B��B�Bw�BhsBVBL�B@�B:^B1'BoB+BBB	7B�B{BJBB��B�TB��B�RB��B��B�BffBR�BB�B#�B�BbB
��B
�B
��B
��B
��B
�B
r�B
o�B
p�B
n�B
k�B
VB
?}B
/B
�B
bB
1B
B	��B	�mB	�5B	�B	ɺB	�dB	�3B	��B	��B	�DB	v�B	hsB	`BB	]/B	YB	M�B	<jB	49B	-B	$�B	�B	oB��B��B�B�B�B�yB�mB�ZB�5B�B��BƨBB�qB�^B�LB�9B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�VB�PB�PB�JB�7B�+B�B�%B�1B�PB�\B�\B�\B�PB�DB�=B�=B�=B�bB�{B�{B�{B�uB�oB�VB�JB�DB�=B�7B�1B�+B�%B�B�B~�B}�B{�B~�B}�B}�B�B�%B�B�B~�B�B�B�B�%B�=B�=B�1B�B|�B�%B�1B�7B�bB�\B�VB�bB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�?B�LB�XB�dB�qB�}BBȴB��B��B��B��B��B�B�)B�BB�TB�`B�mB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	+B	DB	JB	VB	VB	bB	{B	�B	�B	�B	�B	�B	"�B	#�B	&�B	(�B	)�B	,B	1'B	33B	49B	:^B	B�B	B�B	B�B	E�B	F�B	F�B	F�B	I�B	L�B	N�B	Q�B	S�B	W
B	XB	[#B	]/B	_;B	aHB	aHB	cTB	ffB	e`B	m�B	m�B	o�B	v�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�=B	�VB	�uB	�{B	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	�{B	�hB	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�^B	�jB	�jB	�jB	�qB	�dB	�}B	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�;B	�BB	�HB	�TB	�ZB	�ZB	�fB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
DB
+B
\B
�B
"�B
+B
33B
9XB
>wB
?}B
F�B
L�B
R�B
YB
_;B
cTB
iyB
k�B
p�B
t�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�{B�pB�vB�pB�rB�|B�B�B�B�B�B��B;]Bk�B��B�"B�sBȲB�B��B �BuB.B41B,B.B3*B3+B2(B2%B1B*�B �B�BNBKB	4B"BBB��B��B�B�kB�6B��B��B�}B�GB� B��B��B�!Bx�BiqBWBM�BA�B;aB2$BkB*BBB
4B�BzBHB"B��B�RB��B�PB��B��B�BgeBS�BC�B$�B�BaB
��B
�B
��B
B
��B
�B
s�B
p�B
q�B
o�B
l�B
V�B
@xB
0B
}B
_B
	4B
B	��B	�iB	�2B	�B	ʳB	�`B	�0B	��B	��B	�AB	w�B	ipB	a>B	^*B	ZB	N�B	=bB	52B	.B	%�B	 �B	iB��B��B�B�B�xB�qB�bB�SB�/B�B��BǢBÉB�iB�YB�FB�4B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�tB�oB�`B�RB�JB�LB�AB�5B�"B�	B�B�)B�JB�WB�RB�TB�IB�<B�4B�5B�6B�`B�wB�tB�wB�lB�iB�PB�DB�>B�4B�1B�+B�#B�B�B��B�B~�B|�B�B~�B~�B�B�B�B�B�B�B�B�B�B�8B�6B�,B�B}�B�B�+B�0B�]B�XB�PB�\B�jB�kB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�8B�CB�SB�\B�jB�xBÇBɮB��B��B��B��B��B�B�#B�:B�LB�^B�fB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	 �B	B	&B	?B	FB	MB	OB	\B	tB	�B	�B	�B	�B	�B	#�B	$�B	'�B	)�B	*�B	-B	2#B	4/B	53B	;XB	C�B	C�B	C�B	F�B	G�B	G�B	G�B	J�B	M�B	O�B	R�B	T�B	X B	YB	\B	^*B	`9B	bFB	bEB	dOB	gbB	fZB	n�B	n�B	p�B	w�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�8B	�QB	�sB	�tB	�|B	�B	�uB	�wB	��B	��B	��B	��B	��B	��B	�uB	�eB	�iB	�nB	�qB	�tB	�xB	�yB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�	B	�B	�B	�B	�B	�*B	�YB	�hB	�gB	�fB	�nB	�^B	�{B	ĕB	ƛB	ʸB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�%B	�7B	�@B	�CB	�OB	�RB	�XB	�`B	�pB	�jB	�kB	�lB	�vB	�rB	�wB	�zB	�~B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
B
B
B
B
B
B
B
B
G�O�B
)B
YB
�B
#�B
,B
42B
:UB
?tB
@yB
G�B
M�B
S�B
ZB
`:B
dRB
jxB
l�B
q�B
u�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0), vertically averaged dS =0.001(+/-0.002) in PSS-78.                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940002019060409400020190604094000  AO  ARCAADJP                                                                    20160305201616    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160305201616  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160305201616  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094000  IP                  G�O�G�O�G�O�                