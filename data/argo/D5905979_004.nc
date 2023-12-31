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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141336  20220204114410  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؂{Bly1   @؂�z @6㕁$��c�1&�x�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffB  B   B(  B0  B8  B?��BH  BP��BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�)D�d�D���D���D��D�eqD���D�� D��D�W
D��D��D��D�iHDڗ\D�ڏD�%qD�\�D�)D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B33B33B��B��B'��B/��B7��B?fgBG��BP��BWfgB_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��C �C�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?ٙCA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_ٙCa�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��DvgD��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D�gD|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>�3D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]�gD^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt�Dy��D�
�D�c3D�� D��GD�=D�c�D��D��fD�3D�UpD���D��zD�D�g�Dڕ�D���D�#�D�[3D�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��;A��/A��;A��;A��;A��;A��;A��HA��TA��TA��HA��HA��HA��TA��`A��`A��TA��yA��;A��HA��`A��yA��A̙�A�1'A�
=A�S�A��A�oA�JA�C�A�jA���A��^A��+A�I�A��hA�v�A���A���A�|�A�/A�=qA�ZA��A�9XA���A��yA��\A��A��`A�`BA�A�~�A���A��mA���A���A�p�A�hsA���A��+A�|�A��/A�ffA�Q�A��`A���A��-A�O�A���A��uA���A���A��mA�A�A��A��^A��`A��HA���A���A�`BA�Q�A�l�A�M�A�r�A�=qA�5?A�$�A��+A��9A�`BA�
=A�x�A�A�VA���A�n�A�K�A�$�A���A��A�z�A�hsA���A�ȴA��A���A���A�A}"�AyXAvQ�At�`As�#As"�ApI�Al��Ak��Aj��Aj�uAjE�Ait�Ah�Ag�Ad�`Ab$�AaoA`z�A_t�A]%A\-AZI�AX^5AV�RAU�ATȴAS�7ARjAQ7LAO�AN��AN�\AM��AMp�AMVAK��AKC�AJQ�AH�yAH(�AFE�ADv�AC�-AB��AAdZA@1'A>A=&�A<�!A<ffA< �A:��A:  A9|�A8�A7A6ȴA5�A3�A3K�A3oA1O�A0{A/�;A/�A/XA.��A,��A+&�A)�;A)x�A(��A(bA'�A'XA&�uA%�A$n�A#�FA#��A#�7A#;dA"A�A!\)A ��A�AVAXAr�A�^AƨAVA�Al�A�9A�PAĜA�uA�A��A�A�Al�A�uA;dA	�TA	%AffA�;A��A7LA^5A1'A�FA\)A��A��A/A�RAJA�TA�7A �@���@��@���@� �@��+@��F@�l�@�S�@�~�@��h@��m@�=q@��@���@��;@��#@�9@�1@�t�@��@���@�F@��@�j@߅@�{@ݺ^@���@�9X@���@���@���@�Z@�K�@�~�@��@Ԭ@Ӯ@�v�@�-@���@���@�hs@�ƨ@�O�@�1'@���@�n�@�hs@���@�j@ǍP@�o@ư!@�~�@���@�p�@ă@�\)@�{@�?}@� �@�?}@�S�@���@���@��j@��u@�j@�"�@�{@��@�Q�@���@�l�@���@���@��F@�V@���@��!@�V@���@�7L@���@���@��@�"�@�+@�;d@���@��-@���@�Q�@�ƨ@���@��!@���@��+@�^5@�=q@�J@��T@��#@��#@���@�O�@��/@�bN@�I�@�9X@�A�@�9X@��F@�~�@��@�&�@�b@��m@�ƨ@�
=@�n�@��@���@��-@�x�@�X@���@�j@��m@�|�@��;@�ƨ@��@�t�@��@���@�E�@�J@���@��@��j@��9@��9@�z�@�9X@� �@���@��@���@�E�@��@��j@��@���@��j@�9X@��P@�;d@��@��!@���@��\@��+@�v�@�ff@�M�@��@��^@�`B@��/@���@�j@�r�@�Q�@�Q�@�1'@���@���@��@���@��@�S�@�"�@���@���@���@���@�~�@�v�@�ff@�M�@�=q@�5?@�$�@�@���@���@�J@�{@���@�@���@��7@���@��@��9@�j@��;@�33@���@��\@�ȴ@��y@�~�@�E�@�-@�{@�@�{@�E�@�^5@�5?@�-@�-@�-@�-@�5?@�=q@�5?@�-@�{@��@��-@��7@��@���@�hs@�O�@�?}@�V@���@�9X@�b@�1@��@���@��F@��P@�dZ@�C�@�"�@�
=@�^�@y��@q�t@jd�@c��@\PH@SP�@NE�@GU�@>YK@9[W@3��@.�'@+C�@%��@��@�@e�@�@�@
B[111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��;A��/A��;A��;A��;A��;A��;A��HA��TA��TA��HA��HA��HA��TA��`A��`A��TA��yA��;A��HA��`A��yA��A̙�A�1'A�
=A�S�A��A�oA�JA�C�A�jA���A��^A��+A�I�A��hA�v�A���A���A�|�A�/A�=qA�ZA��A�9XA���A��yA��\A��A��`A�`BA�A�~�A���A��mA���A���A�p�A�hsA���A��+A�|�A��/A�ffA�Q�A��`A���A��-A�O�A���A��uA���A���A��mA�A�A��A��^A��`A��HA���A���A�`BA�Q�A�l�A�M�A�r�A�=qA�5?A�$�A��+A��9A�`BA�
=A�x�A�A�VA���A�n�A�K�A�$�A���A��A�z�A�hsA���A�ȴA��A���A���A�A}"�AyXAvQ�At�`As�#As"�ApI�Al��Ak��Aj��Aj�uAjE�Ait�Ah�Ag�Ad�`Ab$�AaoA`z�A_t�A]%A\-AZI�AX^5AV�RAU�ATȴAS�7ARjAQ7LAO�AN��AN�\AM��AMp�AMVAK��AKC�AJQ�AH�yAH(�AFE�ADv�AC�-AB��AAdZA@1'A>A=&�A<�!A<ffA< �A:��A:  A9|�A8�A7A6ȴA5�A3�A3K�A3oA1O�A0{A/�;A/�A/XA.��A,��A+&�A)�;A)x�A(��A(bA'�A'XA&�uA%�A$n�A#�FA#��A#�7A#;dA"A�A!\)A ��A�AVAXAr�A�^AƨAVA�Al�A�9A�PAĜA�uA�A��A�A�Al�A�uA;dA	�TA	%AffA�;A��A7LA^5A1'A�FA\)A��A��A/A�RAJA�TA�7A �@���@��@���@� �@��+@��F@�l�@�S�@�~�@��h@��m@�=q@��@���@��;@��#@�9@�1@�t�@��@���@�F@��@�j@߅@�{@ݺ^@���@�9X@���@���@���@�Z@�K�@�~�@��@Ԭ@Ӯ@�v�@�-@���@���@�hs@�ƨ@�O�@�1'@���@�n�@�hs@���@�j@ǍP@�o@ư!@�~�@���@�p�@ă@�\)@�{@�?}@� �@�?}@�S�@���@���@��j@��u@�j@�"�@�{@��@�Q�@���@�l�@���@���@��F@�V@���@��!@�V@���@�7L@���@���@��@�"�@�+@�;d@���@��-@���@�Q�@�ƨ@���@��!@���@��+@�^5@�=q@�J@��T@��#@��#@���@�O�@��/@�bN@�I�@�9X@�A�@�9X@��F@�~�@��@�&�@�b@��m@�ƨ@�
=@�n�@��@���@��-@�x�@�X@���@�j@��m@�|�@��;@�ƨ@��@�t�@��@���@�E�@�J@���@��@��j@��9@��9@�z�@�9X@� �@���@��@���@�E�@��@��j@��@���@��j@�9X@��P@�;d@��@��!@���@��\@��+@�v�@�ff@�M�@��@��^@�`B@��/@���@�j@�r�@�Q�@�Q�@�1'@���@���@��@���@��@�S�@�"�@���@���@���@���@�~�@�v�@�ff@�M�@�=q@�5?@�$�@�@���@���@�J@�{@���@�@���@��7@���@��@��9@�j@��;@�33@���@��\@�ȴ@��y@�~�@�E�@�-@�{@�@�{@�E�@�^5@�5?@�-@�-@�-@�-@�5?@�=q@�5?@�-@�{@��@��-@��7@��@���@�hs@�O�@�?}@�V@���@�9X@�b@�1@��@���@��F@��P@�dZ@�C�@�"�G�O�@�^�@y��@q�t@jd�@c��@\PH@SP�@NE�@GU�@>YK@9[W@3��@.�'@+C�@%��@��@�@e�@�@�@
B[111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB<jB;dB;dB;dB<jB<jB<jB<jB<jB:^BN�B�=B��B��B�B�B�)B�NB�fB�B�B�B�B��BB�B&�B)�B(�B,B9XB>wB?}BD�BH�BM�BXBq�B{�Bx�By�Bo�Bo�Bk�BiyBhsBdZBe`Bl�BjBm�Bt�Bk�BN�B49B;dBF�B%�B�B$�B.B5?BH�BZB`BBaHB_;BYBaHBiyBk�Be`BR�BF�B;dBPBBB��B��B�B�B�wB�B�oB�Bn�BXBT�BP�B>wB"�B�B
��B
�#B
ŢB
��B
��B
�\B
�B
s�B
XB
D�B
8RB
2-B
+B
#�B
1B
B	��B	��B	��B	�B	�B	�`B	�B	ǮB	�qB	�XB	�-B	��B	��B	�hB	�B	w�B	n�B	n�B	hsB	`BB	R�B	F�B	@�B	B�B	C�B	A�B	I�B	D�B	C�B	?}B	9XB	49B	,B	�B	�B	hB	%B	B��B�B�B�B�B�B�B�B�B�B�`B�BB�B�B��B��B��BȴBȴBǮBŢBĜB�qB�XB�LB�FB�9B�!B�B�B��B��B��B��B��B��B��B��B��B��B�oB�PB�+B�By�Bv�Bs�Bq�Bo�Bn�BjBiyBhsBe`BcTB\)BYBS�BL�BH�BE�BD�BC�BB�BA�B?}B@�B?}B?}B=qB>wB;dB;dB8RB6FB5?B5?B6FB5?B49B49B5?B49B33B33B33B1'B1'B/B/B/B0!B/B0!B/B/B.B/B1'B1'B2-B2-B33B2-B49B49B6FB5?B7LB7LB:^B:^B:^B;dB;dB:^B:^B:^B:^B:^B>wBG�BI�BM�BR�BT�BVBYB\)B]/B]/B^5B_;B_;B`BBcTBe`Be`BhsBhsBhsBl�Bl�Bm�Bm�Bm�Bp�Bq�Bs�Bs�Bs�Bs�Bu�Bw�B|�B�B�+B�\B�uB��B��B��B��B��B��B��B��B��B�!B�3B�jBBŢBƨBȴBɺB��B��B��B��B��B��B��B��B�
B�B�B�B�B�B�B�/B�;B�HB�`B�fB�yB�B�B�B�B�B��B��B��B��B��B	  B	+B	
=B	
=B	
=B	JB	PB	PB	PB	PB	hB	oB	{B	�B	�B	�B	�B	�B	#�B	'�B	'�B	.B	2-B	7LB	8RB	:^B	?}B	A�B	B�B	C�B	D�B	D�B	D�B	E�B	E�B	F�B	F�B	J�B	K�B	P�B	VB	W
B	YB	\)B	^5B	^5B	_;B	_;B	_;B	aHB	dZB	e`B	ffB	hsB	jB	n�B	p�B	p�B	r�B	s�B	t�B	v�B	w�B	y�B	{�B	� B	�B	�%B	�7B	�JB	�PB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�FB	�RB	�dB	�wB	B	B	ĜB	ŢB	ƨB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�#B	�/B	�/B	�5B	�BB	�HB	�TB	�TB	�/B	��B
3B
PB
FB
�B
*B
/ B
6�B
=�B
B'B
HB
MjB
Q�B
WsB
\CB
b�B
gmB
mCB
r-B
v+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B33B2-B2-B2-B33B33B33B33B33B1'BE�B�B�WB�QB��B��B��B�B�%B�]B�oB�oB�uB��B��BDB�B �B�B"�B0B52B68B;WB?nBD�BN�BhbBr�Bo�Bp�BfVBfVBb>B`2B_,B[B\BcDBa9BdJBkuBb?BE�B*�B2"B=eB�BsB�B$�B+�B?rBP�BV�BXBU�BO�BXB`5BbAB\BI�B=gB2$BB��B��B��B�B�pB��B�AB��B�=By�BeiBN�BK�BG�B5LB�BrB
�B
� B
��B
��B
��B
�?B
x�B
j�B
N�B
;�B
/<B
)B
!�B
�B	�B	�B	��B	��B	��B	�B	�B	�QB	�B	��B	�fB	�MB	�#B	��B	��B	�`B	x B	n�B	e�B	e�B	_oB	W?B	I�B	=�B	7�B	9�B	:�B	8�B	@�B	;�B	:�B	6}B	0XB	+:B	#	B	�B	�B	lB�*B�B��B�B�B�B�B�B�B�B�B�B�hB�KB�&B�B�B��B��B��B��B��B��B��B�}B�eB�YB�SB�FB�.B�)B�)B�
B��B��B��B��B��B��B��B��B��B��B�aB~=B{+Bp�Bm�Bj�Bh�Bf�Be�Ba�B`�B_�B\uBZjBS?BP.BKBC�B?�B<�B;�B:�B9�B8�B6�B7�B6�B6�B4�B5�B2~B2~B/lB-`B,YB,ZB-aB,ZB+TB+TB,ZB+TB*NB*NB*NB(CB(CB&7B&7B&7B'=B&8B'>B&8B&8B%1B&8B(DB(DB)JB)JB*PB)JB+VB+VB-cB,\B.iB.iB1{B1{B1{B2�B2�B1{B1{B1{B1{B1{B5�B>�B@�BD�BJBLBM BP3BSEBTKBTKBUQBVWBVWBW^BZpB\{B\|B_�B_�B_�Bc�Bc�Bd�Bd�Bd�Bg�Bh�Bj�Bj�Bj�Bj�Bl�Bn�Bt	Bx!B~FB�vB��B��B��B��B��B��B��B��B�B�B�9B�KB��B��B��B��B��B��B��B��B��B�B�B�B�B�B� B�-B�-B�-B�-B�&B�3B�EB�QB�^B�uB�{B��B�B�B��B��B��B��B��B��B��B�B�B�>B	PB	PB	PB	]B	cB	cB	cB	cB	{B		�B	�B	�B	�B	�B	�B	�B	�B	B	B	%%B	)>B	.]B	/cB	1nB	6�B	8�B	9�B	:�B	;�B	;�B	;�B	<�B	<�B	=�B	=�B	A�B	B�B	G�B	MB	NB	P&B	S7B	UCB	UCB	VIB	VIB	VIB	XVB	[hB	\nB	]tB	_�B	a�B	e�B	g�B	g�B	i�B	j�B	k�B	m�B	n�B	p�B	r�B	wB	{%B	}1B	�CB	�UB	�[B	�gB	�mB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�0B	�<B	�OB	�[B	�lB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�#B	�)B	�)B	�)B	�5B	�5B	�;B	�HB	�NB	�ZG�O�B	�4B	��B	�7B
TB
JB
�B
!B
&B
-�B
4�B
9)B
?B
DlB
H�B
NuB
SDB
Y�B
^nB
dDB
i.B
m+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144102022020411441020220204114410  AO  ARCAADJP                                                                    20200618141336    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141336  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141336  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114410  IP                  G�O�G�O�G�O�                