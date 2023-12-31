CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-06T02:19:20Z AOML 3.0 creation; 2016-05-31T19:14:49Z UW 3.1 conversion     
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
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160506021920  20190604094001  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_147                   2C  D   APEX                            5368                            041511                          846 @ש����1   @ש�r���@3z�G��d1�S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy3D�	�D�9�D�p D��3D��fD�I�D�|�D���D� D�9�D�p Dǩ�D��D�I�D�s3D�� D��fD�FfD� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @<��@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��DtvgDy D� D�8 D�nfD���D���D�H D�{3D��3D�fD�8 D�nfDǨ D� D�H D�q�D��fD���D�D�D�~fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AЇ+AЅAЅAЇ+AЇ+AЇ+AЅAЇ+AЉ7AЉ7AЋDAЉ7AЇ+A�~�A�G�A���A��AΛ�A�z�A�~�A�hsA�C�A��A͸RA�S�A��A�ƨA�$�A���Aʛ�A�r�A�Q�A�VA���A���A�\)A�A�A�&�A�A�AƸRA�
=A��A�\)A�-A�K�A��DA��A�jA�1'A�"�A�&�A��TA���A�A��FA�E�A��yA�r�A���A�r�A�M�A�
=A���A�`BA��uA�z�A�9XA�VA�{A�v�A��jA�n�A��^A���A��+A��A���A�=qA���A�^5A��9A���A��A���A��A�VA�{A��!A�jA���A�z�A���A��uA��A�ĜA�VA�O�A���A�$�A��A�(�A��A���A��A���A�ƨA�hsA�;A}p�Ay��Au��Arz�Apr�AmƨAgdZAd�jAc�AcS�A^ffA[�AZbNAYoAW��AVM�AT��AS�AO�mAM"�AL$�AJn�AG��AF�!AD��AC�7AB�A@1A>z�A=�A;�#A;;dA:ȴA8�A6�9A5�A2��A0�\A/|�A.1A,��A,9XA+��A+dZA+\)A+�A*�\A)�TA)O�A(�A(-A'p�A'+A&��A%dZA#VA!|�A ��A �jA z�A�Ar�AƨA;dA��Av�A��A��An�A�A5?At�A+A�`A��A��A^5A�;A|�AoAv�Ap�A��A�wAĜA|�AȴA�!AVA1A�^A
1'A��A�#A+A�A��A��A\)A�A~�A�hA �RA r�@�C�@�@�ȴ@�^5@��@��@���@�?}@��@�9X@��@�5?@�7@��@�F@�"�@��H@�@��@�9@�I�@��@���@��
@�ȴ@�7@�o@�ff@�E�@�-@�h@�A�@���@��@���@���@��/@۶F@�|�@�"�@�ff@١�@�1@��@�x�@��@���@��@��@�/@�@���@�C�@�S�@�;d@���@�J@ț�@�l�@Ų-@ă@��;@��y@��u@�+@��!@��@��D@���@��P@�+@���@�^5@��h@�V@��@�A�@�K�@��H@�n�@��T@���@�`B@�/@�Q�@��m@�"�@��@��T@�$�@�$�@�?}@���@�1'@��@���@��@��+@���@��h@��@�1'@�9X@���@��@�+@��\@�ff@���@�&�@��`@��9@���@�Q�@��w@�;d@�M�@�J@���@�l�@���@��/@�%@���@���@�V@� �@�"�@�@�`B@��@�b@�bN@�1'@��R@�M�@�@��H@��H@�hs@��D@�  @�C�@��@��H@�{@�J@��@�X@��@��@�Ĝ@���@�/@���@��-@�"�@�V@�{@��-@�J@��T@��T@�ff@�M�@�=q@�@���@�p�@�@�p�@�V@�I�@�|�@��!@�v�@��@��7@���@���@��7@���@��m@��P@���@��@�K�@�K�@�dZ@��P@�ƨ@���@�K�@�;d@�o@�
=@�o@�+@�;d@��P@���@���@���@�|�@��@�C�@��@��R@��!@��\@�v�@�=q@�@���@���@�/@�bN@�"�@�ȴ@�E�@���@��T@��T@���@��\@�5?@���@�Ĝ@�b@�ƨ@��@�|�@�;d@�
=@��y@���@���@�~�@�-@�$�@��@�O�@�?}@�/@��@���@�Ĝ@��j@��@��@�I�@�  @��@�\)@�K�@�K�@�"�@�"�@��@��@�"�@��y@��!@��+@�E�@��T@���@�J@�{@�@���@�Ĝ@��u@�Q�@���@�;d@�S�@�+@{C�@s�
@h��@]�T@Y��@PĜ@H��@B~�@;C�@6�y@/�@*^5@$�/@��@7L@O�@��@5?@
��@\)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AЇ+AЅAЅAЇ+AЇ+AЇ+AЅAЇ+AЉ7AЉ7AЋDAЉ7AЇ+A�~�A�G�A���A��AΛ�A�z�A�~�A�hsA�C�A��A͸RA�S�A��A�ƨA�$�A���Aʛ�A�r�A�Q�A�VA���A���A�\)A�A�A�&�A�A�AƸRA�
=A��A�\)A�-A�K�A��DA��A�jA�1'A�"�A�&�A��TA���A�A��FA�E�A��yA�r�A���A�r�A�M�A�
=A���A�`BA��uA�z�A�9XA�VA�{A�v�A��jA�n�A��^A���A��+A��A���A�=qA���A�^5A��9A���A��A���A��A�VA�{A��!A�jA���A�z�A���A��uA��A�ĜA�VA�O�A���A�$�A��A�(�A��A���A��A���A�ƨA�hsA�;A}p�Ay��Au��Arz�Apr�AmƨAgdZAd�jAc�AcS�A^ffA[�AZbNAYoAW��AVM�AT��AS�AO�mAM"�AL$�AJn�AG��AF�!AD��AC�7AB�A@1A>z�A=�A;�#A;;dA:ȴA8�A6�9A5�A2��A0�\A/|�A.1A,��A,9XA+��A+dZA+\)A+�A*�\A)�TA)O�A(�A(-A'p�A'+A&��A%dZA#VA!|�A ��A �jA z�A�Ar�AƨA;dA��Av�A��A��An�A�A5?At�A+A�`A��A��A^5A�;A|�AoAv�Ap�A��A�wAĜA|�AȴA�!AVA1A�^A
1'A��A�#A+A�A��A��A\)A�A~�A�hA �RA r�@�C�@�@�ȴ@�^5@��@��@���@�?}@��@�9X@��@�5?@�7@��@�F@�"�@��H@�@��@�9@�I�@��@���@��
@�ȴ@�7@�o@�ff@�E�@�-@�h@�A�@���@��@���@���@��/@۶F@�|�@�"�@�ff@١�@�1@��@�x�@��@���@��@��@�/@�@���@�C�@�S�@�;d@���@�J@ț�@�l�@Ų-@ă@��;@��y@��u@�+@��!@��@��D@���@��P@�+@���@�^5@��h@�V@��@�A�@�K�@��H@�n�@��T@���@�`B@�/@�Q�@��m@�"�@��@��T@�$�@�$�@�?}@���@�1'@��@���@��@��+@���@��h@��@�1'@�9X@���@��@�+@��\@�ff@���@�&�@��`@��9@���@�Q�@��w@�;d@�M�@�J@���@�l�@���@��/@�%@���@���@�V@� �@�"�@�@�`B@��@�b@�bN@�1'@��R@�M�@�@��H@��H@�hs@��D@�  @�C�@��@��H@�{@�J@��@�X@��@��@�Ĝ@���@�/@���@��-@�"�@�V@�{@��-@�J@��T@��T@�ff@�M�@�=q@�@���@�p�@�@�p�@�V@�I�@�|�@��!@�v�@��@��7@���@���@��7@���@��m@��P@���@��@�K�@�K�@�dZ@��P@�ƨ@���@�K�@�;d@�o@�
=@�o@�+@�;d@��P@���@���@���@�|�@��@�C�@��@��R@��!@��\@�v�@�=q@�@���@���@�/@�bN@�"�@�ȴ@�E�@���@��T@��T@���@��\@�5?@���@�Ĝ@�b@�ƨ@��@�|�@�;d@�
=@��y@���@���@�~�@�-@�$�@��@�O�@�?}@�/@��@���@�Ĝ@��j@��@��@�I�@�  @��@�\)@�K�@�K�@�"�@�"�@��@��@�"�@��y@��!@��+@�E�@��T@���@�J@�{@�@���@�Ĝ@��u@�Q�@���@�;d@�S�@�+@{C�@s�
@h��@]�T@Y��@PĜ@H��@B~�@;C�@6�y@/�@*^5@$�/@��@7L@O�@��@5?@
��@\)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ƨB
ŢB
ƨB
�B
�B
��B1BbB{B�B�B)�BN�B[#BaHB^5BcTBhsBn�B�B�\B�'BȴB��B��B�B�B/B[#Bu�B��B�B�B�B��B��B��B�LB��B�fB�`B�)B��B��B�}B�'B��B��BɺB�LB��B�B�9B�B�9B�FBB��BŢB��Br�B�B�HBŢB�!B��B�hB�LB�B�5B��B�RB�3B�-B��B�bB�+B�Bv�BgmB]/BN�BC�B8RB,B#�B�B%B
�B
�BB
��B
B
�B
��B
�bB
{�B
ZB
6FB
&�B
�B
B	�B	ŢB	�wB	�FB	��B	�%B	~�B	v�B	l�B	bNB	W
B	L�B	:^B	,B	#�B	�B	JB	B��B��B�B�`B�;B�B�B��B��BɺBB�wB�?B�B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B�oB�PB�=B�7B�1B�+B�%B�B�B�%B�B�B�B~�B|�Bz�Bz�B{�B{�B{�B{�Bz�Bz�Bw�Bw�B|�B~�B� B}�Bz�Bv�Bp�Bm�Bm�Bm�Bn�Bl�Bl�Bq�Bn�Bk�Bk�Bk�Bl�Bl�Bl�Bl�Bk�Bm�Bl�Bl�Bl�Bk�BjBl�Bm�Bl�BjBhsBk�Bp�Br�Br�Bq�Bw�B� B�B�B�%B�%B�+B�1B�1B�7B�+B�JB�VB�\B�\B�bB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�!B�'B��B�B�XB��BŢBŢBƨBǮB��BɺBɺBǮBǮBȴBȴB��B��B��B��B�B�
B�
B�)B�5B�NB�fB�B�B�B�B�B�B�B��B��B	  B	  B	  B	B	B	PB	hB	�B	�B	#�B	%�B	&�B	%�B	'�B	)�B	)�B	.B	1'B	49B	;dB	=qB	@�B	C�B	G�B	M�B	N�B	P�B	P�B	Q�B	R�B	T�B	VB	W
B	VB	R�B	Q�B	S�B	^5B	cTB	cTB	dZB	iyB	o�B	n�B	n�B	l�B	n�B	p�B	u�B	z�B	v�B	x�B	�B	�B	�B	�B	�B	�B	�B	�+B	�+B	�%B	�1B	�7B	�DB	�PB	�\B	�{B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�?B	�LB	�jB	�qB	�qB	�jB	�dB	�dB	�qB	�wB	�qB	�jB	�jB	�dB	�dB	�dB	�qB	B	B	ĜB	ǮB	B	B	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�5B	�;B	�NB	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�mB	�ZB	�HB	�;B	�BB	�;B	�BB	�NB	�fB	�B	�B	�B	�B	�yB	�yB	�yB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
	7B
VB
!�B
(�B
)�B
6FB
>wB
F�B
M�B
S�B
YB
_;B
cTB
hsB
o�B
r�B
v�B
y�B
}�B
�B
�%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
ǟB
ǞB
ǢB
ǞB
ǠB
ǞB
ǟB
ǟB
ǢB
ǟB
ǟB
ǢB
ǤB
ƚB
ŔB
ƜB
��B
�B
��B%BSBlB�B�B)�BN�B[Ba;B^'BcFBhcBn�B��B�QB�BȦBʰB˻B�tB�B/B[Bu�B��B��B��B�B��B��B��B�=B��B�TB�SB�B��BʴB�rB�B��B��BɪB�@B��B��B�,B�B�.B�=BB̽BŐB��Br�B�B�=BŖB�B��B�YB�AB�B�)B̾B�DB�#B�!B��B�SB�B��Bv�BgbB]!BN�BC�B8EB+�B#�BtBB
�B
�8B
��B
B
��B
�B
�XB
{�B
ZB
67B
&�B
�B
B	�B	ŔB	�kB	�8B	��B	�B	~�B	v�B	lB	b@B	V�B	L�B	:RB	+�B	#�B	�B	=B	B��B��B�B�YB�0B�B��B��B��BɱBB�jB�5B��B��B��B��B��B��B��B��B��B�	B�B��B��B��B��B��B��B�fB�EB�3B�)B�*B�B�B�B�B�B�B�B��B~�B|�Bz�Bz�B{�B{�B{�B{�Bz�Bz�Bw�Bw�B|�B~�B�B}�Bz�Bv�Bp�Bm�Bm�Bm�Bn�Bl�Bl�Bq�Bn�Bk{Bk{BkyBl�Bl{Bl�Bl~Bk|Bm�Bl�Bl�Bl�Bk{BjrBl~Bm�Bl�BjuBhjBkwBp�Br�Br�Bq�Bw�B�B��B�B�B�B�!B�'B�&B�)B�"B�@B�KB�PB�QB�[B�ZB�fB�uB�B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B�B�MB�~BŖBŖBƞBǣBʶBɰBɱBǦBǣBȫBȦB˻B��B��B��B��B�B��B� B�*B�CB�\B�~B�B�B�}B�{B�B�B��B��B��B��B��B	B	B	CB	]B	�B	�B	#�B	%�B	&�B	%�B	'�B	)�B	)�B	.
B	1B	4.B	;XB	=gB	@wB	C�B	G�B	M�B	N�B	P�B	P�B	Q�B	R�B	T�B	U�B	V�B	U�B	R�B	Q�B	S�B	^(B	cGB	cLB	dPB	inB	o�B	n�B	n�B	l�B	n�B	p�B	u�B	z�B	v�B	x�B	�B	�B	�B	�B	��B	��B	��B	�!B	�#B	�B	�'B	�,B	�7B	�FB	�RB	�nB	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�5B	�>B	�^B	�gB	�gB	�]B	�XB	�YB	�iB	�nB	�eB	�bB	�`B	�XB	�XB	�YB	�bB	B	B	đB	ǣB	B	B	ĐB	ēB	ŚB	ƠB	ǤB	ȪB	ʳB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�,B	�,B	�CB	�VB	�YB	�dB	�dB	�bB	�cB	�hB	�gB	�iB	�eB	�fB	�OB	�AB	�1B	�8B	�0B	�7B	�BB	�]B	�B	�B	�B	�yB	�nB	�oB	�nB	�iB	�hB	�tB	�oB	�mB	�uB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
 B
 �B
B
B
�B
B
B
B
B
B
B
 B
 �B
 �B
 B
B
	-B
KB
!�B
(�B
)�B
6:B
>kB
F�B
M�B
S�B
Y	B
_1B
cIB
hjB
o�B
r�B
v�B
y�B
}�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =0(+/-0.002) in PSS-78.                                                                                                                                                                                                 Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940012019060409400120190604094001  AO  ARCAADJP                                                                    20160506021920    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160506021920  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160506021920  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094001  IP                  G�O�G�O�G�O�                