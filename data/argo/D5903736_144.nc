CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-05T09:15:55Z AOML 3.0 creation; 2016-05-31T19:14:48Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  yl   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20160405091555  20190604094000  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_144                   2C  D   APEX                            5368                            041511                          846 @עFt>5�1   @עGB@3�I�^�d7
=p��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DsٚDy� D��D�9�D�� D�� D�3D�S3D�y�D�� D���D�#3D��3D��3D���D�9�Dڐ D��fD��D�33D�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds�gDy��D�3D�8 D�~fD��fD��D�Q�D�x D��fD��3D�!�D���D���D�� D�8 DڎfD���D�3D�1�D� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aϟ�Aκ^A͇+A�=qA�-A��A��A�VA�%A���A��A��mA��HA��/A��
A���A�ȴA̲-A�~�A�Q�Aˏ\A�7LA���Aʝ�Aʡ�A�v�Aʏ\A�VA��A�&�A���AɼjA���AɅA�A�x�A�I�A��A���A�~�A�$�A��AŴ9A��
A¸RA�bA�v�A�S�A���A�33A��/A�JA���A�$�A�9XA���A��FA��^A�bNA��wA�"�A��\A��A��A�v�A��FA��A���A���A��A�A�A�Q�A�%A�Q�A��RA�XA��A�1A�^5A�%A��+A��A���A��A�C�A���A���A��A�x�A��A�M�A�v�A�{A�1A��A�G�A���A�ffA�?}A��
A���A�5?A��TA���A� �A��`A���A��TAoA|VAz��AzffAw&�At�Arn�Aq�^Ao�TAkt�Ai33Ai
=Af�Ae
=AchsAb5?Aa�TAaS�A`ȴA_�FA^r�A\jAX�RAWK�AVM�AU"�AR�\AQ�AQoAO��AN-AM�-AM?}AKAI�7AGAE�AB��A@$�A=�#A<=qA;l�A9�mA9+A8v�A7�^A6�/A6�RA6n�A4VA3&�A1�
A0JA/�A.��A.ffA,�A,�A*n�A)33A(=qA'��A'"�A&��A&-A%hsA$�A#�A!�A n�A{A�A��AO�AoA��AA�A+A{A�A�hA�!AbAO�A��AȴAQ�A��A&�A
=qA	��A	C�A�jAQ�A33A�;A�A1A��A��A�A��A�/A�A��A �!A 1@��@�M�@��@�l�@�ȴ@���@���@�  @�?}@�P@�-@�\)@�/@�G�@�{@�@땁@�R@�dZ@��H@�V@��@�b@�E�@�p�@�@�dZ@�&�@�=q@��@���@�p�@݁@�&�@�bN@��#@υ@�+@��@��;@�v�@ɉ7@�?}@��m@ƸR@��#@���@Ĭ@�Z@Å@\@���@���@�1@�S�@��@��!@�v�@��@��@�C�@���@���@�X@�&�@���@�Q�@� �@���@���@�5?@��T@��#@���@���@��^@���@�p�@��u@���@�C�@�-@�J@�$�@��#@��7@�=q@��\@�~�@�=q@�=q@�J@���@�X@���@���@��u@��@�j@�Z@�A�@��m@���@�;d@��y@�ȴ@�v�@�{@��@�hs@��`@�9X@��@�v�@�=q@�5?@��@�p�@���@���@� �@�33@�@���@�V@�A�@���@���@�v�@�V@�@�%@��@�Q�@��@���@�33@���@���@�V@�$�@�{@�@�G�@���@��@�Z@�(�@�  @��F@�o@���@��\@�v�@�J@���@���@�&�@���@��@���@�Ĝ@��u@�z�@�Q�@�I�@��;@���@��!@��R@�
=@��@�+@��+@�5?@�{@��^@�&�@�1@���@��w@�t�@�"�@�ȴ@���@�M�@��@���@�hs@�G�@��@�V@���@���@��9@��D@�bN@��D@�7L@�%@��D@�1'@��m@��
@�b@��@�I�@��@��`@��D@��D@���@�I�@���@��@�K�@���@��R@��\@��+@�ff@�n�@���@�v�@�=q@�=q@��H@�n�@��@���@�/@��u@��@�ƨ@�"�@���@�v�@�M�@�5?@�{@��@���@��h@�p�@���@�`B@��@�&�@���@���@���@�j@�I�@� �@��@��P@�dZ@�S�@�t�@�33@�
=@�ȴ@��+@�v�@�-@���@��@��9@��@�bN@�Q�@�n�@~v�@w�P@m@ct�@Z�!@Sƨ@K�m@CdZ@<��@8bN@2�!@-?}@'�;@ �9@�@��@@@+@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aϟ�Aκ^A͇+A�=qA�-A��A��A�VA�%A���A��A��mA��HA��/A��
A���A�ȴA̲-A�~�A�Q�Aˏ\A�7LA���Aʝ�Aʡ�A�v�Aʏ\A�VA��A�&�A���AɼjA���AɅA�A�x�A�I�A��A���A�~�A�$�A��AŴ9A��
A¸RA�bA�v�A�S�A���A�33A��/A�JA���A�$�A�9XA���A��FA��^A�bNA��wA�"�A��\A��A��A�v�A��FA��A���A���A��A�A�A�Q�A�%A�Q�A��RA�XA��A�1A�^5A�%A��+A��A���A��A�C�A���A���A��A�x�A��A�M�A�v�A�{A�1A��A�G�A���A�ffA�?}A��
A���A�5?A��TA���A� �A��`A���A��TAoA|VAz��AzffAw&�At�Arn�Aq�^Ao�TAkt�Ai33Ai
=Af�Ae
=AchsAb5?Aa�TAaS�A`ȴA_�FA^r�A\jAX�RAWK�AVM�AU"�AR�\AQ�AQoAO��AN-AM�-AM?}AKAI�7AGAE�AB��A@$�A=�#A<=qA;l�A9�mA9+A8v�A7�^A6�/A6�RA6n�A4VA3&�A1�
A0JA/�A.��A.ffA,�A,�A*n�A)33A(=qA'��A'"�A&��A&-A%hsA$�A#�A!�A n�A{A�A��AO�AoA��AA�A+A{A�A�hA�!AbAO�A��AȴAQ�A��A&�A
=qA	��A	C�A�jAQ�A33A�;A�A1A��A��A�A��A�/A�A��A �!A 1@��@�M�@��@�l�@�ȴ@���@���@�  @�?}@�P@�-@�\)@�/@�G�@�{@�@땁@�R@�dZ@��H@�V@��@�b@�E�@�p�@�@�dZ@�&�@�=q@��@���@�p�@݁@�&�@�bN@��#@υ@�+@��@��;@�v�@ɉ7@�?}@��m@ƸR@��#@���@Ĭ@�Z@Å@\@���@���@�1@�S�@��@��!@�v�@��@��@�C�@���@���@�X@�&�@���@�Q�@� �@���@���@�5?@��T@��#@���@���@��^@���@�p�@��u@���@�C�@�-@�J@�$�@��#@��7@�=q@��\@�~�@�=q@�=q@�J@���@�X@���@���@��u@��@�j@�Z@�A�@��m@���@�;d@��y@�ȴ@�v�@�{@��@�hs@��`@�9X@��@�v�@�=q@�5?@��@�p�@���@���@� �@�33@�@���@�V@�A�@���@���@�v�@�V@�@�%@��@�Q�@��@���@�33@���@���@�V@�$�@�{@�@�G�@���@��@�Z@�(�@�  @��F@�o@���@��\@�v�@�J@���@���@�&�@���@��@���@�Ĝ@��u@�z�@�Q�@�I�@��;@���@��!@��R@�
=@��@�+@��+@�5?@�{@��^@�&�@�1@���@��w@�t�@�"�@�ȴ@���@�M�@��@���@�hs@�G�@��@�V@���@���@��9@��D@�bN@��D@�7L@�%@��D@�1'@��m@��
@�b@��@�I�@��@��`@��D@��D@���@�I�@���@��@�K�@���@��R@��\@��+@�ff@�n�@���@�v�@�=q@�=q@��H@�n�@��@���@�/@��u@��@�ƨ@�"�@���@�v�@�M�@�5?@�{@��@���@��h@�p�@���@�`B@��@�&�@���@���@���@�j@�I�@� �@��@��P@�dZ@�S�@�t�@�33@�
=@�ȴ@��+@�v�@�-@���@��@��9@��@�bN@�Q�@�n�@~v�@w�P@m@ct�@Z�!@Sƨ@K�m@CdZ@<��@8bN@2�!@-?}@'�;@ �9@�@��@@@+@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
ǮB
ȴB
ŢB
ƨB
ŢB
ĜB
ĜB
ĜB
ĜB
ÖB
B
B
B
B
B
B
B
�wB
�qB
�qB
��B
ƨB
�B,B9XBR�BG�BM�BgmBaHBffBk�Bm�BgmBm�Bv�B�+B��B�BB�)BB�B2-B49B5?B5?BH�B^5Be`BBƨB��B�B��B��B�B�B�B�B�'B�3B�FBÖBƨBÖB�}B�9B��B��B��B�=Bo�BL�BD�B<jB1'B�B
=B�yB�BB�
BŢB�^B��Bp�BR�BC�B8RB%�B.B�B�B�BoB
�B
�5B
��B
�-B
�-B
�3B
�-B
��B
��B
�{B
�oB
�bB
�JB
y�B
e`B
ZB
P�B
;dB
%�B
�B
\B	��B	�BB	��B	��B	�qB	�B	��B	��B	��B	�uB	�VB	�1B	}�B	o�B	]/B	T�B	O�B	I�B	?}B	;dB	5?B	/B	'�B	$�B	 �B	�B	oB	DB	B��B�B�ZB�5B�B��B��B��B��BɺBȴBŢB�}B�^B�?B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�jB�)B�yB�B�B�B�B�B�B�fB�HB�BɺB��B�B��B��B��B�{B�hB�=B�B�B{�B{�B�B��B�FB�dB�-B��B��B��B��B��B��B��B��B��B�PB�bB�oB��B�-B�wB��B��B��B�}BĜB�)B�HB�ZB�;B�B�B�)B�B�B��B��B�B�HB�B��B��B��B��B�B��B��B��B��B��B��B�B�B�B�B�'B�-B�3B�FB�RB�jB��BÖBŢBǮBȴBɺB��B��B��B�
B�B�#B�)B�/B�5B�5B�;B�ZB�fB�mB�mB�mB�sB�sB�sB�sB�B�B�B��B��B��B��B	  B	DB	hB	oB	{B	�B	�B	�B	�B	�B	 �B	"�B	"�B	$�B	&�B	(�B	,B	-B	/B	1'B	33B	5?B	9XB	:^B	;dB	;dB	=qB	A�B	B�B	C�B	C�B	C�B	F�B	I�B	N�B	Q�B	R�B	R�B	Q�B	P�B	R�B	R�B	S�B	W
B	XB	XB	XB	YB	YB	YB	ZB	]/B	_;B	aHB	bNB	dZB	e`B	ffB	jB	l�B	o�B	p�B	q�B	r�B	t�B	y�B	{�B	{�B	|�B	� B	�B	�B	�%B	�=B	�JB	�PB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�!B	�B	�B	�B	�9B	�FB	�dB	�qB	�qB	�qB	�qB	�wB	�qB	�wB	�}B	��B	��B	��B	��B	ĜB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�/B	�5B	�BB	�HB	�NB	�NB	�NB	�HB	�BB	�BB	�HB	�ZB	�`B	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
%B
1B
	7B

=B
DB
\B
�B
�B
%�B
/B
6FB
;dB
A�B
G�B
N�B
Q�B
ZB
_;B
e`B
jB
n�B
q�B
t�B
w�B
z�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�mB
�PB
�QB
�DB
�EB
�DB
�;B
�;B
�=B
�=B
�4B
�.B
�1B
�1B
�.B
�2B
�1B
�/B
�B
�B
�B
�)B
�MB
� B,�B9�BS�BHOBNuBhBa�Bg
Bl$Bn2BhBn3BwmB��B�B��B�1B��B�B2B2�B4�B5�B5�BI\B^�BfB�6B�LB�-B��B��B��B��B��B��B��B��B��B��B�:B�MB�<B�!B��B�rB�TB�7B��Bp@BMrBEAB=B1�BKB
�B�B��BתB�EB�B�NBqDBS�BD;B8�B&�B.�B ^B.BQBB
�?B
��B
�sB
��B
��B
��B
��B
�4B
�;B
�B
�B
�B
��B
z{B
e�B
Z�B
Q�B
<B
&�B
4B
�B	��B	��B	�{B	�hB	�B	��B	�wB	�@B	�,B	�B	��B	��B	~�B	p<B	]�B	U�B	P}B	JYB	@B	<B	5�B	/�B	(�B	%{B	!dB	3B	B	�B	�B�kB�*B��B��BڻBՙB҈B�}B�kB�ZB�TB�@B�B��B��B��B��B��B��B��B�zB�dB�aB�VB�LB�EB�FB�FB�dB��B��B�wB�xB�XB�FB�%B� B�B��B�B�$B�(B�)B�+B�)B�B�B��B֢B�[B�(B��B�LB�9B�$B�B�B��B��B��B|�B|�B��B�XB��B� B��B��B��B��B��B��B�B�\B�KB�*B��B�B�B�rB��B�BуBтB�_B�B�=B��B��B��B��BھBھB��BڼBجBӓB�aBخB��B֣B՞B՝BЀB�)B��B�sB�pB��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�&B�4B�=B�NB�VB�XB�gB�sB՟BצBٷB��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�)B�=B�?B�dB�uB��B��B	 �B	�B	B	B	B	$B	:B	FB	SB	 ^B	!fB	#lB	#pB	%{B	'�B	)�B	,�B	-�B	/�B	1�B	3�B	5�B	9�B	; B	< B	<B	>B	B)B	C.B	D4B	D7B	D4B	GJB	JZB	OxB	R�B	S�B	S�B	R�B	Q�B	S�B	S�B	T�B	W�B	X�B	X�B	X�B	Y�B	Y�B	Y�B	Z�B	]�B	_�B	a�B	b�B	d�B	f B	gB	kB	m+B	p>B	qDB	rMB	sOB	u[B	z|B	|�B	|�B	}�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�1B	�;B	�NB	�VB	�\B	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�!B	�"B	�,B	�;B	�BB	�CB	�UB	�rB	�{B	�xB	�rB	�tB	�{B	цB	ҏB	ԘB	׫B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�9B	�>B	�?B	�=B	�;B	�1B	�,B	�%B	�!B	�B	�B	�*B	�2B	�8B	�=B	�FB	�WB	�[B	�iB	�kB	�bB	�jB	�oB	�sB	�{B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B
�B
�B
/B
YB
&�B
/�B
6�B
<B
B+B
HOB
OwB
R�B
Z�B
_�B
fB
k B
o9B
rGB
u^B
xpB
{�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =0.001(+/-0.002) in PSS-78.                                                                                                                                                                                             Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940002019060409400020190604094000  AO  ARCAADJP                                                                    20160405091555    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160405091555  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160405091555  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094000  IP                  G�O�G�O�G�O�                