CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:24Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170924  20220204114424  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���j@91   @�ގ8�@70��
=q�b�ȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy^�D�=D�J=D��{D��
D��D�W�D���D��=D��D�_\D��{D��qD��D�aHDڐ�D��{D�\D�^fD�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�ff@�ffA ��A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bp33Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL�3DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�DtɚDy[�D��D�H�D���D��pD�D�VD��)D�أD�D�]�D���D���D�D�_�Dڏ
D���D��D�\�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��A���A��
A��/A��HA��#A��/A��A��#A��HA��A�"�A�E�A��\A���A���A���A��yA��
A���A��jA��A��uA�hsA�C�A�9XA�-A�"�A�oA��/A��A��A�jA�1'A���A��HA��RA�z�A�1'A��A���A�Q�A��TA�A�A��jA�v�A�`BA��A���A��FA�-A�n�A��A�Q�A�7LA�ȴA���A�"�A���A���A�"�A�K�A�=qA���A�ĜA��^A���A�E�A���A�$�A�ƨA�jA�9XA��A�hsA��#A�33A�hsA�bNA���A�Q�A���A���A���A�r�A�K�A�VA�/A���A��A���A���A���A��A�1A��RA��A�|�A�hsA�+A���A��uA�K�A�^5A���A�O�A��A��A���A�hsA�I�A��A�33A���A��#A�5?A��TA�I�A�XA�
=A��wA�A~~�Az��AyC�Av��As�Aq��Ao��Am��Ak"�AgƨAe�Ae/Ac��AcoA`�A]��A]"�A[��AY��AVȴAUK�AS��AQ�AP�AOVAL�AJ��AI��AG�AE��AC��AAXA?A>v�A=�A<�A;C�A8~�A7��A5;dA3��A3K�A2VA1ƨA1��A1/A0 �A/O�A.�9A-�
A,bA*n�A)��A(�A'�TA&n�A%�A$��A#��A"�!A ��AK�AhsAJA�A�A9XAdZA{A\)A�AA�HAbNA�^AffA�-A�uA��A&�A�+A�AS�AQ�A�AȴA�+A�A\)A
=AAG�A
-A�/Av�AZAA�A�wAĜAbAhsA�!A{A33A ��A �DA Z@���@�=q@�p�@�K�@�&�@� �@�C�@�@�7@�`B@�bN@�n�@���@�p�@��@��@�%@�u@�t�@�V@�/@�|�@�@�{@���@߾w@�^5@�t�@���@�-@���@�A�@׍P@�o@��@��@��
@�v�@ѩ�@�9X@��H@�/@�t�@ɡ�@�I�@�o@�n�@��#@Ų-@Ł@�Ĝ@��@�  @Õ�@��#@�Ĝ@�I�@��@��H@���@�5?@��@��9@���@�\)@��#@�V@�9X@�ƨ@��@��H@���@�M�@��-@�hs@�&�@��`@�K�@��@�p�@���@���@��@��D@�r�@�Q�@���@��@��\@�@��h@�X@�7L@��@���@��D@�S�@�~�@���@��@�O�@���@��9@��@��9@�b@�o@�M�@�{@���@���@��7@�`B@���@���@�Q�@��@�1@��@��H@��@��!@�;d@��@���@�v�@�J@�?}@��/@��/@��@�t�@��@��@�M�@�M�@�J@��@�x�@��@��u@�  @��F@�|�@��@���@��@��H@���@�-@�J@�{@�V@��/@���@���@���@��@��@�z�@�Ĝ@���@�I�@���@�ƨ@��P@�S�@�"�@�@���@�ȴ@�ȴ@��+@�V@���@��-@���@��@�?}@�?}@�`B@�?}@�O�@�/@��@��D@��u@�Z@� �@�b@�(�@�b@�b@���@���@��;@��
@�
=@�5?@��@���@��h@�`B@��@�V@��@�bN@��D@��D@��@��u@��@��y@���@���@�dZ@��@�ȴ@�M�@�7L@��9@�1'@� �@�b@��
@���@�C�@��y@���@���@�~�@�^5@�=q@�=q@�v�@�V@���@��T@���@�x�@�G�@�7L@�?}@�7L@�n�@�@�@���@��j@��#@�p�@��@���@�@��#@��h@�`B@�&�@�Ĝ@�z@���@x��@q�z@hM@`z�@Wqv@Qzx@I�H@C��@>5?@9[W@3)_@-c@'�+@"$�@W�@�j@E9@b�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��TA��A���A��
A��/A��HA��#A��/A��A��#A��HA��A�"�A�E�A��\A���A���A���A��yA��
A���A��jA��A��uA�hsA�C�A�9XA�-A�"�A�oA��/A��A��A�jA�1'A���A��HA��RA�z�A�1'A��A���A�Q�A��TA�A�A��jA�v�A�`BA��A���A��FA�-A�n�A��A�Q�A�7LA�ȴA���A�"�A���A���A�"�A�K�A�=qA���A�ĜA��^A���A�E�A���A�$�A�ƨA�jA�9XA��A�hsA��#A�33A�hsA�bNA���A�Q�A���A���A���A�r�A�K�A�VA�/A���A��A���A���A���A��A�1A��RA��A�|�A�hsA�+A���A��uA�K�A�^5A���A�O�A��A��A���A�hsA�I�A��A�33A���A��#A�5?A��TA�I�A�XA�
=A��wA�A~~�Az��AyC�Av��As�Aq��Ao��Am��Ak"�AgƨAe�Ae/Ac��AcoA`�A]��A]"�A[��AY��AVȴAUK�AS��AQ�AP�AOVAL�AJ��AI��AG�AE��AC��AAXA?A>v�A=�A<�A;C�A8~�A7��A5;dA3��A3K�A2VA1ƨA1��A1/A0 �A/O�A.�9A-�
A,bA*n�A)��A(�A'�TA&n�A%�A$��A#��A"�!A ��AK�AhsAJA�A�A9XAdZA{A\)A�AA�HAbNA�^AffA�-A�uA��A&�A�+A�AS�AQ�A�AȴA�+A�A\)A
=AAG�A
-A�/Av�AZAA�A�wAĜAbAhsA�!A{A33A ��A �DA Z@���@�=q@�p�@�K�@�&�@� �@�C�@�@�7@�`B@�bN@�n�@���@�p�@��@��@�%@�u@�t�@�V@�/@�|�@�@�{@���@߾w@�^5@�t�@���@�-@���@�A�@׍P@�o@��@��@��
@�v�@ѩ�@�9X@��H@�/@�t�@ɡ�@�I�@�o@�n�@��#@Ų-@Ł@�Ĝ@��@�  @Õ�@��#@�Ĝ@�I�@��@��H@���@�5?@��@��9@���@�\)@��#@�V@�9X@�ƨ@��@��H@���@�M�@��-@�hs@�&�@��`@�K�@��@�p�@���@���@��@��D@�r�@�Q�@���@��@��\@�@��h@�X@�7L@��@���@��D@�S�@�~�@���@��@�O�@���@��9@��@��9@�b@�o@�M�@�{@���@���@��7@�`B@���@���@�Q�@��@�1@��@��H@��@��!@�;d@��@���@�v�@�J@�?}@��/@��/@��@�t�@��@��@�M�@�M�@�J@��@�x�@��@��u@�  @��F@�|�@��@���@��@��H@���@�-@�J@�{@�V@��/@���@���@���@��@��@�z�@�Ĝ@���@�I�@���@�ƨ@��P@�S�@�"�@�@���@�ȴ@�ȴ@��+@�V@���@��-@���@��@�?}@�?}@�`B@�?}@�O�@�/@��@��D@��u@�Z@� �@�b@�(�@�b@�b@���@���@��;@��
@�
=@�5?@��@���@��h@�`B@��@�V@��@�bN@��D@��D@��@��u@��@��y@���@���@�dZ@��@�ȴ@�M�@�7L@��9@�1'@� �@�b@��
@���@�C�@��y@���@���@�~�@�^5@�=q@�=q@�v�@�V@���@��T@���@�x�@�G�@�7L@�?}@�7L@�n�@�@�@���@��j@��#@�p�@��@���@�@��#@��h@�`B@�&�G�O�@�z@���@x��@q�z@hM@`z�@Wqv@Qzx@I�H@C��@>5?@9[W@3)_@-c@'�+@"$�@W�@�j@E9@b�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��BB�B+B@�BL�BXB]/B_;BcTBe`BiyBo�Bs�Bt�Bv�Bx�Bz�B� B�B�7B�DB�oB��B��B��B��B�B�9B�XB��B��B�B�ZB�yB�B�B�B�B��BB
=BuB�B�B,B2-B33B;dB@�BC�BG�BR�BVBW
BXBYBXB\)B[#B\)B[#B\)B_;B\)BXBZBVBL�BI�BE�BA�B=qB<jB;dB1'B#�B �B�B�B%B�`B�B��B�XB�B��B��B�hB�PB�JB�DB� Bt�Bm�BXBR�BJ�B>wB<jB49B#�BDB
�B
��B
�!B
��B
�JB
u�B
]/B
e`B
]/B
@�B
1'B
�B
%B	�B	�TB	��B	�wB	��B	�uB	�VB	�B	{�B	k�B	W
B	S�B	I�B	>wB	(�B	�B	uB	%B��B��B�B�B��B�qB�!B��B��B��B��B�B�B�B��B��B��B�PB�=B�1B�VB��B��B��B��B�oB�JB�B�B}�B�B�B|�B{�B}�B� Bx�Bm�BdZBm�Bo�BjBt�By�B}�B~�B{�Bz�B{�B{�Bz�B{�By�Bv�Bw�Bv�Bw�Bv�Bv�Bu�Bs�Bp�Bp�Bs�Bw�Bt�Bs�Br�Bu�Bp�Bm�B\)B_;BcTBk�BjBk�BhsBhsBiyBhsBhsBgmBgmBgmBhsBffBhsBgmBgmBe`BaHB]/B^5BbNBcTBcTBbNBcTBdZBdZBcTBcTBbNB`BB^5B[#B]/B[#BXBR�BM�BL�BT�BT�BS�BYBZB\)B[#B\)BZBZB[#B[#B_;BbNBaHBaHBaHBaHBbNBcTBdZBffBk�Bp�Br�Bt�Bu�Bu�Bx�Bx�By�Bz�B}�B�B�B�%B�+B�VB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�?B�LB�RB�XB�^B�^B�dB�qBÖBȴB��B��B��B��B��B�B�B�#B�5B�BB�;B�BB�BB�BB�NB�TB�`B�`B�mB�B�B��B��B��B��B	B	B	B	%B		7B	DB	VB	hB	uB	{B	�B	�B	!�B	%�B	'�B	)�B	,B	,B	/B	33B	5?B	=qB	>wB	?}B	A�B	C�B	G�B	G�B	G�B	J�B	J�B	J�B	K�B	N�B	O�B	Q�B	S�B	T�B	[#B	_;B	^5B	_;B	`BB	aHB	bNB	cTB	dZB	ffB	hsB	jB	k�B	m�B	p�B	p�B	q�B	r�B	s�B	t�B	w�B	{�B	{�B	~�B	� B	�B	�B	�%B	�+B	�=B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�FB	�LB	�LB	�RB	�FB	�?B	�?B	�FB	�LB	�LB	�LB	�FB	�FB	�LB	�RB	�^B	�jB	�wB	��B	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	�
B	�B	�#B	�/B	�HB	�`B	�`B	�`B	�`B	�ZB	�!B	��B
zB
:B
�B
)�B
)�B
0�B
5�B
=�B
EB
IB
O�B
UMB
[	B
a�B
d�B
m�B
r�B
v`B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�
B	}B�B3tB?�BJ�BPBR)BVBBXNB\fBb�Bf�Bg�Bi�Bk�Bm�Br�BwB|!B~.B�YB�|B��B��B��B�B� B�>B�iB��B��B�<B�[B�mB�B�B�B�B��B�BTB
kB�B�B%	B&B.@B3^B6qB:�BE�BH�BI�BJ�BK�BJ�BOBM�BOBM�BOBRBOBJ�BL�BH�B?�B<�B8�B4hB0PB/IB.DB$B�B�B�BeB�B�JB�B��B�GB�B��B��B�\B�DB?B~9Br�Bg�B`�BKBE�B=�B1uB/hB'8B�B
�GB
�B
��B
�.B
��B
[B
h�B
PEB
XvB
PFB
3�B
$CB
�B	�FB	��B	�yB	�B	��B	�B	��B	��B	u<B	oB	^�B	J?B	G.B	<�B	1�B	1B	�B	�B�dB�:B�B��B�aB�$B��B�jB�B�B�4B�.B�YB�_B�MB�B��B��B��B}�B{�B��B��B��B��B��B��B�BwjBtXBqFBtXBu^BpABo:BqGBsSBl)B`�BW�B`�Bb�B]�BhBm0BqIBrOBo<Bn6Bo<Bo<Bn7Bo=Bm1BjBk&Bj Bk&Bj Bj BiBgBc�Bc�BgBk'BhBgBfBiBc�B`�BO�BR�BV�B^�B]�B^�B[�B[�B\�B[�B[�BZ�BZ�BZ�B[�BY�B[�BZ�BZ�BX�BT�BP�BQ�BU�BV�BV�BU�BV�BW�BW�BV�BV�BU�BS�BQ�BN�BP�BN�BKpBFSBA4B@/BH_BH_BGYBLxBM~BO�BN�BO�BM~BM~BN�BN�BR�BU�BT�BT�BT�BT�BU�BV�BW�BY�B^�BdBfBhBi#Bi#Bl5Bl5Bm;BnABqTBvqBx~By�Bz�B��B��B��B��B�B�"B�"B�B�B�"B�4B�4B�@B�FB�MB�MB�SB�YB�YB�_B�}B��B��B��B��B��B��B��B��B��B�B�'B�-B�9B�9B�QB�]B�pB�|BэBӚBғBӚBӚBӚBզB֬BظBظB��B��B�B�B� B� B�DB�nB�gB�gB�zB��B��B	�B	�B	�B	�B	
�B	�B	B	5B	BB	NB	ZB	ZB	"mB	&�B	(�B	0�B	1�B	2�B	4�B	6�B	:�B	:�B	:�B	>B	>B	>B	?B	B(B	C.B	E;B	GGB	HLB	NqB	R�B	Q�B	R�B	S�B	T�B	U�B	V�B	W�B	Y�B	[�B	]�B	^�B	`�B	c�B	c�B	d�B	e�B	gB	hB	kB	o2B	o2B	rEB	sKB	uWB	xiB	yoB	zuB	}�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�0B	�=B	�IB	�IB	�CB	�CB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�0B	�0B	�)B	�#B	�#B	�MB	�`B	�fB	�rB	ԊB	آB	آB	آB	آG�O�B	�bB	�"B	��B
yB
�B
B
�B
#�B
)5B
1B
8\B
<@B
C6B
H�B
NEB
T�B
W�B
aB
fB
i�B
m0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.012(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144242022020411442420220204114424  AO  ARCAADJP                                                                    20200619170924    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170924  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170924  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114424  IP                  G�O�G�O�G�O�                