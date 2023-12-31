CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-05-20T07:01:40Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20170520070140  20190604094027  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @���@�1   @��E7@4$���S��d�V�u1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` DxٚD�{D�L)D�� D�ָD��D�EqD�w�D���D��D�5qD��\D�θD� D�C�Dړ3D��fD�
D�G�D� D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ff@�ffA33A?33A_33A33A���A���A���A�fgAϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cv�Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��DvgD��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D%3D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt\�Dx�gD�
�D�J�D�~fD��D�  D�C�D�vD��RD��zD�3�D���D��D�fD�A�Dڑ�D���D�pD�FD�fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AӴ9A�x�A��A���A��HA�ƨAҲ-Aҝ�AҍPAҁA�t�A�|�A҅Aҕ�A�oA�M�AЇ+A�%A�9XAͩ�A���A�1A˛�AʃAɛ�A�x�A�ffA�VA��A�-A�oAőhA��`A��#A�%A���A� �A�S�A�ȴA��TA��`A��A�1A��!A�VA���A��7A�|�A��FA�E�A�ƨA��A��A��/A��;A��A��jA�`BA�ƨA�M�A�A���A�A��A���A��A��A��A���A��A��A��A��uA��
A�l�A�(�A��A�/A��;A�S�A��A�A�A���A�A���A��A��PA�/A��^A�^5A�1A��wA���A�"�A���A�S�A�E�A�ȴA�x�A�bA���A�Q�A�oA�v�A��A�r�A�1A��RA�oA~�`A}�Az�DAx  AvjAu�Atn�Ast�Ar�Arz�Aq�Ao%An�uAkAfbNAe�hAe?}Ab�!A`bNA_A\��AZ��AY�^AXz�AU�-AT�!AT1'ASdZARffAP~�AN�ALbAKO�AJ�AJA�AJ�AIAI
=AH^5AG��AE��AC�AC"�AA��AA7LAAA@�jA@��A@=qA>�9A<ĜA:�A9|�A9hsA8n�A6�A5�A3;dA2�A2bNA1��A0�/A0Q�A/ƨA/7LA.5?A,�DA+��A*1A(��A(ZA&Q�A$�yA$A�A#dZA"�uA"  A!�wA!hsA��AO�A �AffA��A��AbA�Az�A{A��AQ�A�A�FA�A��A��A�AVA��AA�A`BA�A�#A��A�A��A��A9XA
ZA	�7Av�A��A\)AA�RAI�A��A/A��AI�A�A�A�A
=A ��A �jA �\@�-@���@��;@��@�\)@��H@�A�@��@��@��m@�b@�@�P@�C�@�~�@�V@���@��@�!@�=q@�j@��@��@ޗ�@܃@���@�1'@�"�@��/@�C�@�V@���@���@��m@��@�E�@�7L@�^5@�%@�Ĝ@�A�@ư!@Ł@���@ģ�@��@�
=@�@�X@��@��@�1@��\@�p�@��@�@��@��!@��R@���@���@�E�@�x�@�bN@���@�C�@��+@�M�@���@�?}@��j@��@�z�@�r�@�r�@�j@�j@�1@�$�@�b@���@�n�@�-@���@��@�(�@��@��@��@�t�@�S�@�C�@�33@�33@�+@��@�o@���@��@��@�v�@�@���@���@�O�@���@�Ĝ@��@�Z@�  @��@���@��P@��@�|�@�l�@�dZ@�dZ@�K�@�+@�@���@���@�n�@�-@���@�p�@��`@��9@��D@�r�@�(�@�b@��@��m@��m@��@�+@��y@�=q@���@�hs@�V@�Ĝ@��9@���@��@�Z@�1@��;@��F@�|�@�
=@��\@�M�@���@���@���@���@��@�z�@�A�@�ƨ@���@�C�@�@��H@�ȴ@�ȴ@��R@���@���@�n�@��@��T@�@���@�?}@���@���@�A�@���@��@�@��H@���@�ȴ@�ȴ@���@���@��R@���@�E�@�J@��@���@���@��@�hs@�O�@��@��@�1'@��m@���@��w@��@�S�@���@�V@�{@���@���@��@��@��#@��^@��-@���@�7L@��@���@��@�t�@�K�@�33@��H@���@�~�@�=q@��-@��7@��@�7L@��@�9X@�1@�ƨ@���@�l�@��@��y@���@�n�@�^5@�V@�E�@�5?@�{@��@�O�@�!-@�@�@}w2@r@g;d@a��@Y�@T�P@J�b@A��@=�h@7=@0�/@)�@$PH@"L0@M�@�@k�@�@
҉11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AӴ9A�x�A��A���A��HA�ƨAҲ-Aҝ�AҍPAҁA�t�A�|�A҅Aҕ�A�oA�M�AЇ+A�%A�9XAͩ�A���A�1A˛�AʃAɛ�A�x�A�ffA�VA��A�-A�oAőhA��`A��#A�%A���A� �A�S�A�ȴA��TA��`A��A�1A��!A�VA���A��7A�|�A��FA�E�A�ƨA��A��A��/A��;A��A��jA�`BA�ƨA�M�A�A���A�A��A���A��A��A��A���A��A��A��A��uA��
A�l�A�(�A��A�/A��;A�S�A��A�A�A���A�A���A��A��PA�/A��^A�^5A�1A��wA���A�"�A���A�S�A�E�A�ȴA�x�A�bA���A�Q�A�oA�v�A��A�r�A�1A��RA�oA~�`A}�Az�DAx  AvjAu�Atn�Ast�Ar�Arz�Aq�Ao%An�uAkAfbNAe�hAe?}Ab�!A`bNA_A\��AZ��AY�^AXz�AU�-AT�!AT1'ASdZARffAP~�AN�ALbAKO�AJ�AJA�AJ�AIAI
=AH^5AG��AE��AC�AC"�AA��AA7LAAA@�jA@��A@=qA>�9A<ĜA:�A9|�A9hsA8n�A6�A5�A3;dA2�A2bNA1��A0�/A0Q�A/ƨA/7LA.5?A,�DA+��A*1A(��A(ZA&Q�A$�yA$A�A#dZA"�uA"  A!�wA!hsA��AO�A �AffA��A��AbA�Az�A{A��AQ�A�A�FA�A��A��A�AVA��AA�A`BA�A�#A��A�A��A��A9XA
ZA	�7Av�A��A\)AA�RAI�A��A/A��AI�A�A�A�A
=A ��A �jA �\@�-@���@��;@��@�\)@��H@�A�@��@��@��m@�b@�@�P@�C�@�~�@�V@���@��@�!@�=q@�j@��@��@ޗ�@܃@���@�1'@�"�@��/@�C�@�V@���@���@��m@��@�E�@�7L@�^5@�%@�Ĝ@�A�@ư!@Ł@���@ģ�@��@�
=@�@�X@��@��@�1@��\@�p�@��@�@��@��!@��R@���@���@�E�@�x�@�bN@���@�C�@��+@�M�@���@�?}@��j@��@�z�@�r�@�r�@�j@�j@�1@�$�@�b@���@�n�@�-@���@��@�(�@��@��@��@�t�@�S�@�C�@�33@�33@�+@��@�o@���@��@��@�v�@�@���@���@�O�@���@�Ĝ@��@�Z@�  @��@���@��P@��@�|�@�l�@�dZ@�dZ@�K�@�+@�@���@���@�n�@�-@���@�p�@��`@��9@��D@�r�@�(�@�b@��@��m@��m@��@�+@��y@�=q@���@�hs@�V@�Ĝ@��9@���@��@�Z@�1@��;@��F@�|�@�
=@��\@�M�@���@���@���@���@��@�z�@�A�@�ƨ@���@�C�@�@��H@�ȴ@�ȴ@��R@���@���@�n�@��@��T@�@���@�?}@���@���@�A�@���@��@�@��H@���@�ȴ@�ȴ@���@���@��R@���@�E�@�J@��@���@���@��@�hs@�O�@��@��@�1'@��m@���@��w@��@�S�@���@�V@�{@���@���@��@��@��#@��^@��-@���@�7L@��@���@��@�t�@�K�@�33@��H@���@�~�@�=q@��-@��7@��@�7L@��@�9X@�1@�ƨ@���@�l�@��@��y@���@�n�@�^5@�V@�E�@�5?@�{@��G�O�@�!-@�@�@}w2@r@g;d@a��@Y�@T�P@J�b@A��@=�h@7=@0�/@)�@$PH@"L0@M�@�@k�@�@
҉11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBl�Bp�Br�Bs�Bt�Bt�Bt�Bs�Bu�B�B�PB�{B��B��B�9BB�wB�9B��B�hB�\B��B�'B�}B��B�B�B�B�/B�B��BB
=B!�B5?BF�BO�Bk�B�DB�+B{�B�B�1B�=B�JB�VB�bB��B��B��B��B��B��B��B�{B�VB�JB�=B�7B�1B�+B�7B�1B�B�B{�BhsBR�B@�B49B$�BoB	7BB  B��B�B�sB�`B�/BɺB�9B��B�=B}�Bo�B\)B6FB%�B�B�B�BbBVBB
�B
�`B
�/B
�#B
��B
��B
�FB
�B
��B
��B
��B
�bB
�1B
~�B
ffB
P�B
C�B
49B
&�B
�B
�B
\B
	7B
B
B	��B	�B	�fB	��B	�RB	�dB	��B	�RB	�B	��B	��B	�PB	�1B	�B	w�B	v�B	t�B	p�B	iyB	`BB	XB	L�B	G�B	D�B	B�B	A�B	?}B	=qB	;dB	8RB	/B	#�B	�B	�B	oB	{B	uB	hB	\B		7B	B��B��B��B�B�yB�NB�#B�B�B��B��B��BɺBŢB��B�XB�9B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�9B�?B�9B�^B�}B��BĜBȴBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��BɺBƨBÖBĜBŢBĜBÖB�}B�XB�FB�'B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�B�'B�3B�FB�dB�^B�RB�LB�LB�LB�RB�RB�LB�^B�^B�XB�XB�dB�}B��BBĜBȴB��B��B��B��B��B�
B�#B�TB�fB�mB�yB�yB�yB�yB�B�B��B��B��B	  B	B	+B	JB	bB	oB	oB	oB	oB	oB	oB	uB	�B	�B	#�B	$�B	%�B	&�B	0!B	33B	49B	6FB	7LB	7LB	8RB	9XB	9XB	9XB	:^B	:^B	;dB	=qB	=qB	?}B	C�B	I�B	L�B	M�B	R�B	VB	VB	XB	\)B	`BB	gmB	k�B	m�B	m�B	m�B	n�B	n�B	o�B	q�B	t�B	w�B	z�B	|�B	~�B	~�B	� B	�B	�B	�B	�+B	�+B	�+B	�1B	�1B	�7B	�7B	�=B	�PB	�PB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�FB	�LB	�RB	�RB	�RB	�XB	�dB	�jB	�}B	��B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�BB	�NB	�TB	�TB	�ZB	�`B	�`B	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
1B

=B
�B
�B
!�B
-wB
6`B
:�B
A;B
D�B
M�B
TaB
W�B
\B
`�B
e�B
k�B
n�B
rGB
v�B
z^B
}�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BdBh(Bj4Bk5Bl:Bl>Bl<Bk6BmBBy�B��B��B�(B�gB��B�B��B��B�JB��B��B�8B��B��B�<BЏBГBњBԪB�B�iB��B�BCB,�B>BGSBb�B��B~�Bs[ByB�B��B��B��B��B��B� B�7B�>B�8B�'B�B��B��B��B��B��B�B~�B��B�Bz�By�Bs_B_�BJlB7�B+�B^B	�B �B��B�B�aB�%B��B��BԳB�BB��B�)B��Bu�Bg$BS�B-�BrBPB3BB�B�B
��B
�JB
��B
��B
ҷB
̕B
�jB
��B
��B
��B
�]B
�+B
��B
�B
v�B
^B
H�B
;4B
+�B
�B
NB
0B
B
 �B	��B	��B	�B	�2B	�B	ʛB	�B	�B	�4B	��B	��B	��B	�?B	� B	�B	x�B	o�B	n{B	lmB	hUB	a*B	W�B	O�B	D�B	?fB	<SB	:FB	9BB	74B	5)B	3B	0	B	&�B	�B	fB	:B	
*B	8B	.B		"B	B	 �B��B�B�B�B�dB�6B�B��B��B��BʴBƚBďB�{B�bB�KB�B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�tB�gB�aB�bB�ZB�[B�UB�[B�`B�aB�aB�uB�uB�uB��B�}B��B��B��B��B��B��B��B�	B�B�&B�DB�IB�eB�zB�zB�}B��BBĔBȬBʶBʹBȭBƠBśBĕBēBÎB��B�nB�^B�bB�iB�gB�\B�FB�$B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�4B�*B�B�B�B�B�B�B�B�*B�)B�#B�$B�,B�JB�TB�ZB�hB�}BĘBŜBƢBǩBȰB��B��B�B�0B�9B�AB�?B�EB�CB�OB�fB�B�B�B��B��B��B	B	)B	
7B	
7B	
6B	
4B	
5B	
5B	;B	\B	wB	�B	�B	�B	�B	'�B	*�B	+�B	.B	/B	/B	0B	1B	1B	1B	2 B	2%B	3)B	57B	57B	7AB	;ZB	A}B	D�B	E�B	J�B	M�B	M�B	O�B	S�B	XB	_3B	cEB	eTB	eSB	eUB	f[B	f[B	gdB	ikB	l�B	o�B	r�B	t�B	v�B	v�B	w�B	y�B	{�B	|�B	~�B	~�B	~�B	�B	�B	��B	��B	�B	�B	�B	�B	�6B	�9B	�EB	�LB	�PB	�`B	�lB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�"B	�*B	�<B	�GB	�OB	�VB	�VB	�UB	�ZB	�[B	�aB	�pB	�uB	�vB	B	čB	ƕB	ǝB	ʮB	˵B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	� B	�.B	�7B	�>B	�CB	�?B	�?B	�PB	�RB	�VB	�`B	�aB	�_B	�aB	�_B	�_B	�bB	�bB	�]B	�eB	�jB	�mB	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
mB
�B
PB
%1B
.B
2�B
8�B
<�B
EWB
LB
OyB
S�B
X�B
]fB
c�B
fhB
jB
n�B
rB
uYB
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.008(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940272019060409402720190604094027  AO  ARCAADJP                                                                    20170520070140    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170520070140  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170520070140  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094027  IP                  G�O�G�O�G�O�                