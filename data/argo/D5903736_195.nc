CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-09-10T07:12:37Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170910071237  20190604094029  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�%����1   @�%.u�\@6�S����d�I�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  Ba33Bg��Bo��Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr� Ds  Ds� Dt  Dt� Dt�fDy��D��{D�P�D���D��
D��D�33D��{D�ָD��fD�9�D���Dǿ�D��
D�]qDڄ�D��\D��)D�G
D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��Ba  BgfgBofgBw��B��B��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��B��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3CaٙCc�3Ce�3Cg�3Cj�Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��DvgD��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dr3Dr|�Dr��Ds|�Ds��Dt|�Dt�3Dy��D���D�O\D�\D��pD�
=D�1�D���D��D���D�8RD��=DǾD��pD�[�Dڃ3D���D���D�EpD�D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�E�A��A�\)A��A�JA���A��A��TA���A��A�jA�{A�ĜA�PA�hsA��yA�ZAߧ�A�+A��Aީ�AށA�1Aݲ-Aݕ�A�A�ĜAׁA��;A��/A�^5AЬA�bNA�7LḀ�A���A�ffA�-A�A�A��TA�XA��AżjA�9XA�ZAøRA�x�A�ȴA�-A��PA���A�~�A�r�A�=qA�
=A��/A�ffA�(�A���A���A���A�VA��;A��+A�-A�9XA�=qA�I�A��A��-A��A��A���A�  A�ffA�A���A��^A���A�Q�A��A��TA�oA��mA���A�oA�9XA���A�ffA�1'A�$�A���A�\)A�v�A�
=A��A��RA���A�x�A�I�A���A�%A�G�A��+A��A�x�A��A��uA���A�A�"�A�VA���A�v�A��uA��A���A��\A��A���A��A��yA��!A��hA��7A�+A�hA}C�A{
=AyG�AxVAw|�Av�At�DAq�hAol�Am�mAmVAl1'Aj�!Ah��AhA�Ad�AbE�A`n�A^(�A[S�AZ{AX�`AX1AVz�AT��AR��AO�AO&�AM�7AKK�AJ~�AI�AFE�AE�
AD(�AA�TA@ĜA?�7A>A=?}A<��A;�PA:�HA:��A9��A8  A5�A3��A1|�A0��A/A.ĜA-�;A,VA+XA)�;A'�-A%A$�A#S�A!�7A �A �DA Q�A��A+A��A�-A�HAƨAȴAG�A�+A�A�A`BAVA�DAQ�A1A"�A��A"�AA�A7LA�A+AVA��A%A�+A�;A�A
VA	|�AA�AK�A$�A �A`BA��A  AVA z�@�E�@�dZ@���@��`@�ȴ@��7@��@�ƨ@�ȴ@���@�E�@��`@�(�@�\@�b@�M�@�@�1@◍@�5?@�7L@ߝ�@�M�@ݲ-@���@�Q�@ۥ�@ڗ�@�@�?}@���@��@�5?@թ�@Ԭ@��@��y@Ѻ^@�O�@���@���@�@�?}@���@�S�@�"�@ʏ\@�G�@ǍP@���@�J@�@ř�@��/@�Z@�  @î@�C�@���@��@�hs@��@�ƨ@�33@�"�@��@�o@��@���@��+@�5?@���@�x�@��@�j@� �@�33@��+@��7@�/@���@� �@�C�@��@���@�v�@��7@��@��j@�Z@���@��!@���@���@�?}@��/@�ƨ@�l�@�ȴ@�J@���@���@�z�@�Z@���@���@��@�
=@�n�@�M�@�X@��@���@�Ĝ@�r�@�9X@��
@�C�@���@��+@�-@��@��^@�ƨ@�\)@���@��\@�=q@�-@�v�@��@��`@�bN@���@�"�@��\@�J@�{@���@��h@�7L@��@�V@��/@��@�9X@��@���@��w@�|�@�\)@���@�~�@�$�@���@�O�@�%@���@���@��@�r�@�Q�@�A�@�(�@�  @��@�ƨ@���@�dZ@�@��+@�v�@�~�@�~�@�~�@�v�@�v�@�n�@�n�@�n�@�n�@�v�@�~�@�V@�-@��@��^@��h@��7@��7@�x�@�p�@�X@�/@��@�%@��@� �@�ƨ@�;d@��H@�^5@�J@��#@���@���@��^@��7@�p�@�x�@�X@�%@��j@��u@�r�@�I�@�1'@��@��@�ƨ@���@�l�@�K�@��@��H@��\@�V@��#@�x�@�p�@�X@�%@��@��@���@��`@���@���@�z�@�A�@�  @��w@���@�K�@�33@�o@��y@���@��R@��+@�V@�J@��T@���@���@�hs@��@��@��@��@tC-@l��@ec�@\�@RO@K
=@D�[@>}V@7� @2��@*��@$֡@ 6@~�@
=@��@X�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�+A�E�A��A�\)A��A�JA���A��A��TA���A��A�jA�{A�ĜA�PA�hsA��yA�ZAߧ�A�+A��Aީ�AށA�1Aݲ-Aݕ�A�A�ĜAׁA��;A��/A�^5AЬA�bNA�7LḀ�A���A�ffA�-A�A�A��TA�XA��AżjA�9XA�ZAøRA�x�A�ȴA�-A��PA���A�~�A�r�A�=qA�
=A��/A�ffA�(�A���A���A���A�VA��;A��+A�-A�9XA�=qA�I�A��A��-A��A��A���A�  A�ffA�A���A��^A���A�Q�A��A��TA�oA��mA���A�oA�9XA���A�ffA�1'A�$�A���A�\)A�v�A�
=A��A��RA���A�x�A�I�A���A�%A�G�A��+A��A�x�A��A��uA���A�A�"�A�VA���A�v�A��uA��A���A��\A��A���A��A��yA��!A��hA��7A�+A�hA}C�A{
=AyG�AxVAw|�Av�At�DAq�hAol�Am�mAmVAl1'Aj�!Ah��AhA�Ad�AbE�A`n�A^(�A[S�AZ{AX�`AX1AVz�AT��AR��AO�AO&�AM�7AKK�AJ~�AI�AFE�AE�
AD(�AA�TA@ĜA?�7A>A=?}A<��A;�PA:�HA:��A9��A8  A5�A3��A1|�A0��A/A.ĜA-�;A,VA+XA)�;A'�-A%A$�A#S�A!�7A �A �DA Q�A��A+A��A�-A�HAƨAȴAG�A�+A�A�A`BAVA�DAQ�A1A"�A��A"�AA�A7LA�A+AVA��A%A�+A�;A�A
VA	|�AA�AK�A$�A �A`BA��A  AVA z�@�E�@�dZ@���@��`@�ȴ@��7@��@�ƨ@�ȴ@���@�E�@��`@�(�@�\@�b@�M�@�@�1@◍@�5?@�7L@ߝ�@�M�@ݲ-@���@�Q�@ۥ�@ڗ�@�@�?}@���@��@�5?@թ�@Ԭ@��@��y@Ѻ^@�O�@���@���@�@�?}@���@�S�@�"�@ʏ\@�G�@ǍP@���@�J@�@ř�@��/@�Z@�  @î@�C�@���@��@�hs@��@�ƨ@�33@�"�@��@�o@��@���@��+@�5?@���@�x�@��@�j@� �@�33@��+@��7@�/@���@� �@�C�@��@���@�v�@��7@��@��j@�Z@���@��!@���@���@�?}@��/@�ƨ@�l�@�ȴ@�J@���@���@�z�@�Z@���@���@��@�
=@�n�@�M�@�X@��@���@�Ĝ@�r�@�9X@��
@�C�@���@��+@�-@��@��^@�ƨ@�\)@���@��\@�=q@�-@�v�@��@��`@�bN@���@�"�@��\@�J@�{@���@��h@�7L@��@�V@��/@��@�9X@��@���@��w@�|�@�\)@���@�~�@�$�@���@�O�@�%@���@���@��@�r�@�Q�@�A�@�(�@�  @��@�ƨ@���@�dZ@�@��+@�v�@�~�@�~�@�~�@�v�@�v�@�n�@�n�@�n�@�n�@�v�@�~�@�V@�-@��@��^@��h@��7@��7@�x�@�p�@�X@�/@��@�%@��@� �@�ƨ@�;d@��H@�^5@�J@��#@���@���@��^@��7@�p�@�x�@�X@�%@��j@��u@�r�@�I�@�1'@��@��@�ƨ@���@�l�@�K�@��@��H@��\@�V@��#@�x�@�p�@�X@�%@��@��@���@��`@���@���@�z�@�A�@�  @��w@���@�K�@�33@�o@��y@���@��R@��+@�V@�J@��T@���@���@�hsG�O�@��@��@��@tC-@l��@ec�@\�@RO@K
=@D�[@>}V@7� @2��@*��@$֡@ 6@~�@
=@��@X�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB~�B}�B|�B{�Bz�Bz�Bz�By�By�Bv�Bt�Bq�Bm�BhsBe`BdZBaHBW
BJ�BE�BC�BC�BD�BD�BE�BT�B��B��BBBB�BB�Bm�Bm�B�B��B��B�?BƨBB�qBƨB�B�5B�B�B��B��B��B�B�#B�)B�/B�/B�#B�#B�)B�)B�)B�B�
B�B��B��B��B��B�FB�-B�!B��B��B{�Bv�By�B�B{�Bx�Bv�B{�By�Bp�BYB1'B"�BPBB��B��B�B�HB��BÖB�-B��B��B��B��B�\B�DB�Bs�Bk�BbNBW
B;dB�B�BVBB
��B
�B
�B
�sB
�HB
�
B
��B
��B
��B
ĜB
�FB
��B
��B
��B
��B
��B
��B
�bB
� B
o�B
dZB
^5B
XB
P�B
B�B
/B
�B
�B
\B
	7B	��B	�B	�mB	��B	�dB	�B	��B	�%B	}�B	x�B	r�B	l�B	cTB	VB	H�B	B�B	9XB	/B	+B	$�B	�B	{B	VB	1B	B��B��B��B��B�B�B�B�yB�NB�)B��B��B��BɺBƨBÖB�}B�jB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�VB�PB�DB�7B�+B�B�B�B|�Bz�Bx�Bv�Bt�Bp�Bn�Bl�Bk�BiyBhsBgmBe`BdZBbNBaHB_;B`BB_;B^5B]/B]/B\)B^5BaHBaHB_;B_;B`BB_;B`BB`BB`BB`BBaHB`BB`BBbNBdZBe`BgmBiyBiyBiyBjBk�Bl�Bl�Bl�Bl�Bm�Bm�Bm�Bn�Bp�Bq�Br�Bu�Bw�B{�B}�B~�B�B�7B�DB�VB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�9B�9B�9B�9B�?B�FB�FB�RB�XB�dB�qB�}B�}BÖBƨBɺB��B��B��B��B��B��B�B�5B�BB�NB�ZB�yB�B��B��B��B��B	B	B	+B	DB	hB	{B	{B	�B	�B	�B	�B	!�B	"�B	#�B	&�B	'�B	)�B	+B	-B	.B	0!B	33B	5?B	7LB	8RB	9XB	:^B	9XB	8RB	6FB	8RB	:^B	<jB	A�B	G�B	H�B	M�B	W
B	ZB	]/B	dZB	l�B	p�B	r�B	s�B	u�B	y�B	}�B	�B	�B	�%B	�+B	�1B	�DB	�DB	�PB	�VB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�LB	�XB	�XB	�^B	�^B	�dB	�dB	�jB	�jB	�jB	�jB	�qB	��B	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�;B	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B

=B
	�B
�B
�B
&B
)�B
4B
;�B
D�B
I�B
MPB
R�B
XEB
[�B
_pB
b�B
gB
k�B
q�B
u�B
x�B
|6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bs�Br�Bq�Bp�Bo�Bo�Bo�Bn�Bn�Bk�Bi�Bf�Bb�B]nBZ\BYWBVGBLB?�B:�B8�B8�B9�B9�B:�BI�B��B��B�B��B�B�B7�BbzBb|Bv�B��B��B�%B��B�rB�TB��B�B�B��B��B��B��B��B��B�
B�B�B�B�	B�B�B�B�
B�B��B��B��B��BýB�vB�2B�B�B��B�wBp�Bk�Bn�Bu�Bp�Bm�Bk�Bp�Bn�Be�BNB&B�BHB�B��B��B�B�FB��B��B�0B��B��B��B��B�gB�JBvBh�B`�BW^BLB0{B�B
�BoB
�4B
��B
�B
ߝB
ݏB
�cB
�&B
�B
��B
��B
��B
�dB
�B
��B
��B
��B
��B
��B
��B
u$B
d�B
Y�B
S^B
M6B
FB
7�B
$IB
�B

�B
�B	�fB	�#B	��B	ܣB	�B	��B	�GB	��B	{`B	s1B	nB	g�B	a�B	X�B	KCB	=�B	7�B	.�B	$bB	 FB	%B	�B		�B	�B�uB�\B�DB�-B�B�B��B��B��B��BםB�zB�MB�1B�!B�B��B��B��B��B��B�}B�dB�SB�;B�'B�#B�B�B�B�B�B��B��B��B��B��B��B��B��B��B~�B|�BzyBxlBvaBrKBp=Bn2Bl#BjBe�Bc�Ba�B`�B^�B]�B\�BZ�BY�BW�BV�BT�BU�BT�BS�BR�BR�BQ�BS�BV�BV�BT�BT�BU�BT�BU�BU�BU�BU�BV�BU�BU�BW�BY�BZ�B\�B^�B^�B^�B_�B`�Ba�Ba�Ba�Ba�Bb�Bb�Bb�Bc�BfBgBhBk&Bm1BqJBsWBt[BxuB~�B��B��B��B��B��B��B��B�B�B�B�-B�,B�7B�5B�<B�;B�CB�JB�VB�]B�jB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�	B�B�"B�0B�<B�\B�\B�\B�oBӑBՠB׭BٵB��B��B�B�)B�.B�7B�fB�mB��B	 �B	�B		�B		�B	
�B	�B	B	B	#B	)B	.B	@B	DB	XB	 [B	"dB	#lB	%yB	(�B	*�B	,�B	-�B	.�B	/�B	.�B	-�B	+�B	-�B	/�B	1�B	6�B	=B	>B	C(B	L^B	OrB	R�B	Y�B	a�B	e�B	h B	i
B	kB	o*B	sHB	w\B	yjB	{vB	||B	}�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�7B	�`B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�.B	�7B	�6B	�AB	�TB	�ZB	�cB	�bB	�lB	�mB	�sB	�yB	�yB	ԄB	טB	؞B	٣B	ګB	ڧB	ڭB	ۮB	ܵB	ݼB	ݾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�	B	�B	�$B	�)B	�&B	�2B	�2B	�3B	�1B	�9B	�;B	�?B	�LB	�OB	�LB	�LB	�WB	�ZB	�aB	�dB	�fB	�gB	�fG�O�B	��B
B
B
_B
�B
)KB
1B
9�B
?B
B�B
G�B
M�B
P�B
T�B
W�B
\eB
a3B
f�B
k=B
nB
qx111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.011(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940292019060409402920190604094029  AO  ARCAADJP                                                                    20170910071237    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170910071237  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170910071237  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094029  IP                  G�O�G�O�G�O�                