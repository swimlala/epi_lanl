CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:51Z creation      
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041151  20190604094022  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @׾mޠ#1   @׾nr��f@3+��Q��d<1&�y1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D�qD�T�D���D���D��\D�D�D�}�D��
D��qD�M�D���D��qD��D�"�DډHD��HD��D�>�D�y�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dtc3Dy��D��D�S3D��D��D���D�C3D�|)D��pD���D�L)D��
D���D�	�D�!GDڇ�D�ǮD�\D�=D�x D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aݧ�Aݣ�Aݡ�AݮAݰ!Aݰ!AݮAݺ^Aݴ9AݮA݁Aݗ�A�|�AݑhAݩ�A�XA�-A�JA�A�A��`Aܺ^A܅A��A�G�A���A�x�A��AٸRA�
=A؃A���A�1'A�VAԲ-A�v�Aѕ�AхAї�A��TAϩ�AΗ�A�bNA��/A�  A�O�A��#AɍPA�p�A�A�A���A��TA�z�A�bA�  A�A��A�l�A�I�A�oA�v�A�9XA��A�M�A�33A��A��A���A���A���A��mA�XA��\A�G�A�z�A�bA�1A�VA���A�=qA�;dA��wA��TA��A��A�{A���A�z�A�(�A��;A��A��A�$�A���A�bNA��!A�9XA�p�A�\)A���A���A�1'A�|�A��A�C�A�r�A��A��yA���A�JA��A��jA�S�A�t�A��A��A���A���A��A�ĜA�|�A�r�A�$�A�S�A��mA���A�dZA��yA��TA}l�Az�DAx9XAu%Am�Aj�HAgK�AdȴAa�7A^��A\9XAW�#AU�TAT�AS��AR-APVAO�hAM\)AL-AK�
AIVAG�AE�AC/AAXA?
=A;�A:A9C�A7�A6^5A3�A2$�A1XA0�yA0�RA05?A/��A.{A,n�A+\)A*��A)�A(^5A'`BA%��A$  A"1'A r�AXA�!A�Al�A�wA|�A��AƨA{AĜAXA�\AA�wA��AO�A  AVA��AbA�+A��A	��A�7An�A�HA�wAjA�`Ax�A�AE�A{A�A�A�A�RA ��A �HA ��A �!A ��A  �@�t�@��+@���@��F@��@�l�@���@��@���@���@�@��^@���@�D@�b@��@��/@��
@�o@�$�@�w@�$�@�x�@�%@�Z@�\)@�p�@��@�@��@���@�-@��@���@���@٩�@��
@֧�@��@�V@Ь@�ƨ@��H@·+@�=q@���@�G�@���@̃@�ƨ@�
=@�^5@ɩ�@�Q�@���@�o@�x�@ă@��;@�@���@��@�r�@�  @�-@��7@�%@��@�A�@�K�@��T@��7@�&�@���@���@�o@��^@��@���@��D@� �@��;@�ƨ@�l�@��@�M�@�@���@���@�9X@��m@�S�@���@��T@�`B@���@��j@��
@��;@��F@��\@�{@���@�@�hs@��@��@�9X@�|�@�K�@�"�@��@��+@�v�@�ff@�-@���@�7L@��`@��@��w@�"�@��y@���@���@�ff@�J@�J@���@�`B@�&�@��/@�j@� �@��F@�C�@���@�~�@�@��#@��^@�p�@�O�@��@��/@�9X@��@�1@���@��F@�l�@��y@���@�V@��@��@���@�p�@��`@�r�@�A�@�I�@�I�@��@��;@��@�C�@���@��y@���@�n�@�$�@�x�@�%@�z�@�(�@�b@��@��w@�l�@�33@��@���@���@�~�@�M�@�-@�J@��@��T@���@�&�@��9@���@�z�@�Z@�Q�@�z�@���@��D@�?}@�`B@��7@��@�hs@�G�@�&�@��@�V@���@�Q�@�1'@�(�@��@�t�@�+@��H@�~�@�ff@�M�@�E�@�$�@��T@�hs@�G�@�&�@���@�I�@�1@�ƨ@��P@�t�@�;d@��R@�~�@�M�@�@���@��@��^@��h@�`B@��`@��u@�z�@�bN@�Z@�I�@��m@�t�@���@���@���@�n�@��@�@��@�/@���@���@�9X@��@���@�o@��H@��R@�@x�K@q��@h�|@`�/@Xm�@M�9@H2�@?l�@<֡@7"�@1��@,A�@&��@!IR@��@�	@^�@V@	@
xl11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aݧ�Aݣ�Aݡ�AݮAݰ!Aݰ!AݮAݺ^Aݴ9AݮA݁Aݗ�A�|�AݑhAݩ�A�XA�-A�JA�A�A��`Aܺ^A܅A��A�G�A���A�x�A��AٸRA�
=A؃A���A�1'A�VAԲ-A�v�Aѕ�AхAї�A��TAϩ�AΗ�A�bNA��/A�  A�O�A��#AɍPA�p�A�A�A���A��TA�z�A�bA�  A�A��A�l�A�I�A�oA�v�A�9XA��A�M�A�33A��A��A���A���A���A��mA�XA��\A�G�A�z�A�bA�1A�VA���A�=qA�;dA��wA��TA��A��A�{A���A�z�A�(�A��;A��A��A�$�A���A�bNA��!A�9XA�p�A�\)A���A���A�1'A�|�A��A�C�A�r�A��A��yA���A�JA��A��jA�S�A�t�A��A��A���A���A��A�ĜA�|�A�r�A�$�A�S�A��mA���A�dZA��yA��TA}l�Az�DAx9XAu%Am�Aj�HAgK�AdȴAa�7A^��A\9XAW�#AU�TAT�AS��AR-APVAO�hAM\)AL-AK�
AIVAG�AE�AC/AAXA?
=A;�A:A9C�A7�A6^5A3�A2$�A1XA0�yA0�RA05?A/��A.{A,n�A+\)A*��A)�A(^5A'`BA%��A$  A"1'A r�AXA�!A�Al�A�wA|�A��AƨA{AĜAXA�\AA�wA��AO�A  AVA��AbA�+A��A	��A�7An�A�HA�wAjA�`Ax�A�AE�A{A�A�A�A�RA ��A �HA ��A �!A ��A  �@�t�@��+@���@��F@��@�l�@���@��@���@���@�@��^@���@�D@�b@��@��/@��
@�o@�$�@�w@�$�@�x�@�%@�Z@�\)@�p�@��@�@��@���@�-@��@���@���@٩�@��
@֧�@��@�V@Ь@�ƨ@��H@·+@�=q@���@�G�@���@̃@�ƨ@�
=@�^5@ɩ�@�Q�@���@�o@�x�@ă@��;@�@���@��@�r�@�  @�-@��7@�%@��@�A�@�K�@��T@��7@�&�@���@���@�o@��^@��@���@��D@� �@��;@�ƨ@�l�@��@�M�@�@���@���@�9X@��m@�S�@���@��T@�`B@���@��j@��
@��;@��F@��\@�{@���@�@�hs@��@��@�9X@�|�@�K�@�"�@��@��+@�v�@�ff@�-@���@�7L@��`@��@��w@�"�@��y@���@���@�ff@�J@�J@���@�`B@�&�@��/@�j@� �@��F@�C�@���@�~�@�@��#@��^@�p�@�O�@��@��/@�9X@��@�1@���@��F@�l�@��y@���@�V@��@��@���@�p�@��`@�r�@�A�@�I�@�I�@��@��;@��@�C�@���@��y@���@�n�@�$�@�x�@�%@�z�@�(�@�b@��@��w@�l�@�33@��@���@���@�~�@�M�@�-@�J@��@��T@���@�&�@��9@���@�z�@�Z@�Q�@�z�@���@��D@�?}@�`B@��7@��@�hs@�G�@�&�@��@�V@���@�Q�@�1'@�(�@��@�t�@�+@��H@�~�@�ff@�M�@�E�@�$�@��T@�hs@�G�@�&�@���@�I�@�1@�ƨ@��P@�t�@�;d@��R@�~�@�M�@�@���@��@��^@��h@�`B@��`@��u@�z�@�bN@�Z@�I�@��m@�t�@���@���@���@�n�@��@�@��@�/@���@���@�9X@��@���@�o@��HG�O�@�@x�K@q��@h�|@`�/@Xm�@M�9@H2�@?l�@<֡@7"�@1��@,A�@&��@!IR@��@�	@^�@V@	@
xl11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBuBuBuBuBuBuBuBhBuBuBuBuBuBuBoBuBoB\BVBVBJBDB	7B+B1B1B�B0!B<jB[#Bo�B�B��B�B�3B��B�DB��B�sB�
BƨB�/BB(�B0!B&�B�B%�B<jBC�BG�BK�BN�BA�BI�BQ�BYBe`BgmBjBq�B�%B�oB��B��B��B�-B�RB��BɺB��B��B��B��B��BɺBŢB��B��B��B��BĜB��B��B��BȴBĜB�jB�LB�3B�B��B��B��B��B��B�JB�Bu�Bp�Bm�BgmB_;BS�B@�B"�BPB��B�ZBĜB�B��B��B�oB�=B|�Bm�BVBL�B@�B>wB<jB2-BDB
��B
��B
�B
|�B
k�B
L�B
7LB
$�B
1B	��B	��B	�B	��B	� B	n�B	YB	:^B	0!B	7LB	<jB	7LB	.B	(�B	�B	�B	bB	1B	B��B��B�B�B�`B�;B�5B�5B�#B�#B�#B�B�B�B�B�B�B�B�B��B��B��B��BŢB�}B��B��B�jB�LB�FB�B��B��B��B�B��B�{B�{B�{B�uB�oB�oB�hB�bB�PB�+B�B�B�-B�PBx�Bx�B�B��B�oB�+B}�B�7B�uB�oB�PB�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�!B�B�B�B��B�B�B�'B�'B�-B�'B�B��B��B��B��B��B�B��B��B��B��B��B��B�bB�oB�bB�PB�JB�JB�JB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�RB�wB�}B��BBĜBŢBȴB��B��B��B�B�
B�B�#B�TB�fB�yB�B�B�B��B��B��B	B	%B	+B	1B	DB	PB	\B	hB	{B	�B	�B	�B	 �B	!�B	#�B	&�B	+B	-B	-B	1'B	8RB	?}B	C�B	D�B	E�B	G�B	I�B	I�B	M�B	P�B	Q�B	T�B	VB	YB	[#B	[#B	\)B	]/B	`BB	aHB	bNB	hsB	m�B	n�B	p�B	p�B	q�B	t�B	t�B	v�B	x�B	z�B	|�B	�B	�B	�B	�B	�B	�DB	�bB	�bB	�bB	�bB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�9B	�?B	�?B	�RB	�RB	�RB	�XB	�dB	�jB	�qB	�wB	�wB	�}B	��B	��B	ĜB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�)B	�NB	�`B	�fB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B

=B
DB
DB
DB
DB
DB
JB
PB
PB
\B
\B
hB
�B
�B
+kB
3�B
:�B
?cB
@�B
J#B
OB
T�B
Y�B
^�B
e�B
h�B
l"B
p�B
s�B
w�B
|B
B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BxBwB
mB	`BWBJBSBOB�B.@B:�BYFBm�B�,B��B�(B�PB��B�dB��B�B�(B��B�RB/B'B.>B%B�B#�B:�BA�BE�BI�BL�B?�BG�BPBW4Bc}Be�Bh�Bo�B�BB��B��B��B�	B�GB�oB��B��B��B��B��B��B��B��BþB��B��B��B��B¼B��B��B��B��B¶B��B�hB�JB�:B�B�B��B��B��B�hB�%Bs�Bn�Bk�Be�B]]BRB>�B �BpB�B�}B¾B�4B��B��B��B�\B{Bk�BT'BJ�B>�B<�B:�B0QB	dB
�B
��B
�7B
{B
i�B
J�B
5uB
#B
XB	��B	��B	�EB	��B	~-B	l�B	WDB	8�B	.NB	5yB	:�B	5zB	,AB	'$B	�B	�B	�B	]B�4B�B��B��B�B�B�hB�aB�aB�NB�TB�RB�LB�FB�FB�@B�=B�CB�3B�3B�$B�B�B�B��B��B��B��B��B�{B�tB�GB�B�B�B�AB��B��B��B��B��B��B��B��B��B�B�^B�<G�O�G�O�B��BwBwB�OB��B��B�ZB|%B�hB��B��B�}B��B��B��B��B��B��B��B��B��B��B��B�B�%B�&B�,B�7B�VB�QB�IB�>B�;B�)B�0B�:B�WB�ZB�^B�XB�NB�.B�B�B�B�'B�4B�%B�B�B��B��B��B��B��B��B��B�{B�}B�{B��B��B��B��B��B��B��B��B��B��B�B�B�/B�IB�XB�dB��B��B��B��B��B��B��B��B��B�B�$B�7B�?B�JB�UB�B�B�B�B��B��B� B�B�"B	BB	RB	ZB	bB		qB	B	�B	�B	�B	�B	�B	�B	�B	�B	"	B	%B	)1B	+?B	+?B	/[B	6�B	=�B	A�B	B�B	C�B	E�B	G�B	G�B	LB	OB	PB	S.B	T5B	WGB	YSB	YSB	ZZB	[_B	^uB	_yB	`}B	f�B	k�B	l�B	n�B	n�B	o�B	r�B	r�B	t�B	wB	yB	{B	4B	5B	�=B	�IB	�LB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�2B	�7B	�>B	�PB	�YB	�ZB	�\B	�`B	�eB	�lB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�'B	�*B	�1B	�;B	�BB	�QB	�XB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�B	�	B	�B	�B	�B	� B	�%B	�-B	�+B	�3B	�5B	�6B
AB
GB
NB
LB
KB
KB
WB
XB
^B
]B
^B
dB
mB
	rB
	sB
	qB
	rB
	rB

wB
}B
~B
�B
�G�O�B
"B
B
)�B
1�B
8�B
=�B
?B
HQB
M9B
R�B
W�B
\�B
c�B
gB
jNB
n�B
rB
vB
z-B
}=B
�?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =-0.002(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940222019060409402220190604094022  AO  ARCAADJP                                                                    20181121041151    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041151  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041151  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094022  IP                  G�O�G�O�G�O�                