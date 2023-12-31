CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:55Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170855  20220204114411  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؆d��1   @؆����@7p ě���c�Ƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBG��BO��BX  B`  Bh  BpffBx��B��B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DU� DV  DV� DV��DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D��D�c�D��\D���D�#�D�K3D���D�~D�'
D�Y�D��\D�θD�qD�O
DڑHD�ؤD�*�D�\{D�3D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@�\)@�\)A�A=�A]�A}�A��
A��
A���A��
A��
A��
A��
A��
Bk�Bk�Bk�Bk�B'k�B/k�B7k�B?��BGBOBWk�B_k�Bgk�Bo��Bx8RBB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bõ�Bǵ�B˵�Bϵ�Bӵ�Bׂ�B۵�Bߵ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��>C��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD v�D ��Dv�D��Dv�D��Dv�D��Dv�D�RDv�D��Dv�D��Dv�D��Dv�D��D	v�D	��D
v�D
��Dv�D��Dv�D��Dv�D��Dv�D��Dv�D��Dv�D��Dv�D��Dv�D��Dv�D��Dv�D��Dv�D��Dv�D��DpRD��DpRD��Dv�D��Dv�D��Dv�D��Dv�D��Dv�D��Dv�D��Dv�D��D v�D ��D!v�D!��D"v�D"��D#v�D#��D$v�D$��D%v�D%��D&v�D&�D'v�D'��D(v�D(��D)v�D)��D*v�D*��D+v�D+��D,v�D,��D-v�D-��D.v�D.��D/v�D/��D0v�D0��D1v�D1��D2v�D2��D3v�D3��D4v�D4��D5v�D5��D6v�D6��D7v�D7��D8v�D8��D9v�D9��D:v�D:��D;v�D;��D<v�D<��D=v�D=��D>v�D>��D?v�D?��D@v�D@��DAv�DA��DBv�DB��DCv�DC��DDv�DD��DEv�DE��DFv�DF��DGv�DG��DHv�DH��DIv�DI��DJv�DJ��DKv�DK��DLv�DL��DMv�DM��DNv�DN��DOv�DO��DPv�DP��DQv�DQ��DRv�DR��DSv�DS��DTv�DT�RDUv�DU��DVv�DV�RDWv�DW��DXv�DX��DY}DY��DZv�DZ��D[v�D[��D\v�D\��D]v�D]��D^v�D^��D_v�D_��D`v�D`��Dav�Da��Dbv�Db��Dcv�Dc��Ddv�Dd��Dev�De��Dfv�Df��Dgv�Dg��Dhv�Dh��Div�Di��Djv�Dj��Dkv�Dk��Dlv�Dl��Dmv�Dm��Dnv�Dn��Dov�Do��Dpv�Dp��Dqv�Dq��Drv�Dr��Dsv�Ds��Dtv�Dt��Dy�pD�3D�^�D���D�3D��D�F�D�3D�ypD�"fD�UHD���D��D��D�JfDڌ�D�� D�%�D�W�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A�%A�A�A�
=A��A��A��A� �A�"�A�"�A�$�A�{A�  A���A��Aԧ�A�r�A�A���AӰ!A�^5A�  A���Aѥ�A���A�/A͛�A�ZAǮA���A�JA�A��hA��PA�1'A�{A�r�A�;dA��A��A���A�VA��A��!A�\)A�33A�  A��FA�v�A���A�A�A��jA�JA���A���A�O�A�hsA�oA��A�dZA��A�;dA��A�ffA�+A��DA��jA�E�A���A��TA��A��A��#A���A��wA��A��wA��A��^A��A�7LA���A��A��A�ZA��PA�E�A���A��mA��A���A�S�A���A~~�A}\)A|��A|�Aw��Ar��Ao�-Ak�Aj�\Ah��AgO�AeG�Ac�Ab  A`��A^�uA]�A]l�A\�A\�uA\=qA[��A[�A[p�AZ-AX��AX�\AVȴAT��AShsAR�yAR(�AR1AQp�AO�mANffAM�^AN  AM�wAM&�ALJAJ=qAI/AHE�AG�AE��ADVABA�AAt�A@n�A?�wA?�A>VA=l�A<��A;��A:E�A933A8(�A7��A7t�A6�\A5t�A4 �A3XA2��A1%A/�PA.��A-�A-�A,VA+�A*�A*bNA)ƨA(��A'XA&�uA&-A%hsA$n�A#|�A!�FA r�A�#AAz�At�A�AjAK�A�A�A�HAbA�AhsA?}A��A=qA?}A��A9XA;dA$�Al�A�yA|�A~�A�A��A��A��Ar�At�A
�A	�hA	G�A�AJAA�
A��AE�A�Ax�A��A�^A ��@��@�^5@�@���@��@��w@���@��@�
=@�5?@�z�@���@��@��@�;d@�X@�Z@�v�@�u@�@�@ߕ�@�S�@�"�@�{@�z�@�"�@��@��m@�
=@���@�A�@Ѻ^@� �@�;d@��@ͩ�@�%@�A�@˾w@ˍP@�l�@��H@�I�@�A�@�=q@å�@\@��h@���@�9X@��@���@���@��@��@���@�dZ@�v�@�/@��@���@��m@�;d@��@���@�M�@�@���@�7L@�bN@�b@��;@���@�t�@�|�@�;d@���@��#@�/@�Q�@�K�@��R@�X@���@�1@���@�t�@�33@�ff@���@�O�@�j@�1@���@��
@�K�@�
=@���@��+@�@��h@�/@�b@�C�@�ȴ@���@��\@�M�@��@�@�&�@�1'@�|�@�\)@�ƨ@�(�@�9X@�1@��m@��@�\)@�t�@�;d@��H@�=q@���@���@�p�@���@�bN@��@��F@�S�@�33@�"�@�
=@��@���@�ff@�M�@�$�@���@��#@���@�@���@��-@�hs@�`B@�O�@�?}@�V@��@�A�@��@��@���@��H@��y@��H@���@�M�@���@�@���@���@���@���@���@�@��@��@�V@��`@�r�@��@��F@�t�@�;d@�C�@�
=@��H@�v�@�V@�5?@�J@��@�?}@�?}@�7L@�?}@�&�@�&�@�&�@��@��`@��u@�Q�@��@���@�S�@�"�@�@���@�J@��h@���@��7@��7@��h@���@�G�@�%@���@��`@��/@���@��9@�I�@�9X@��
@��@�33@�@��H@���@��!@���@�ff@�$�@�@��-@�x�@�O�@�X@�x�@�`B@�V@��9@��@��@���@��
@�ƨ@��w@�ƨ@��w@��F@�t�@�C�@��@�
=@���@��@��!@�~�@�=q@��@���@�`B@�7L@�/@��@�V@�]�@}�X@tM@i�@_y�@YA @R{�@I��@DtT@?@6��@1u�@+��@&�@#�@�X@(�@��@��@
�b@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�
=A�%A�A�A�
=A��A��A��A� �A�"�A�"�A�$�A�{A�  A���A��Aԧ�A�r�A�A���AӰ!A�^5A�  A���Aѥ�A���A�/A͛�A�ZAǮA���A�JA�A��hA��PA�1'A�{A�r�A�;dA��A��A���A�VA��A��!A�\)A�33A�  A��FA�v�A���A�A�A��jA�JA���A���A�O�A�hsA�oA��A�dZA��A�;dA��A�ffA�+A��DA��jA�E�A���A��TA��A��A��#A���A��wA��A��wA��A��^A��A�7LA���A��A��A�ZA��PA�E�A���A��mA��A���A�S�A���A~~�A}\)A|��A|�Aw��Ar��Ao�-Ak�Aj�\Ah��AgO�AeG�Ac�Ab  A`��A^�uA]�A]l�A\�A\�uA\=qA[��A[�A[p�AZ-AX��AX�\AVȴAT��AShsAR�yAR(�AR1AQp�AO�mANffAM�^AN  AM�wAM&�ALJAJ=qAI/AHE�AG�AE��ADVABA�AAt�A@n�A?�wA?�A>VA=l�A<��A;��A:E�A933A8(�A7��A7t�A6�\A5t�A4 �A3XA2��A1%A/�PA.��A-�A-�A,VA+�A*�A*bNA)ƨA(��A'XA&�uA&-A%hsA$n�A#|�A!�FA r�A�#AAz�At�A�AjAK�A�A�A�HAbA�AhsA?}A��A=qA?}A��A9XA;dA$�Al�A�yA|�A~�A�A��A��A��Ar�At�A
�A	�hA	G�A�AJAA�
A��AE�A�Ax�A��A�^A ��@��@�^5@�@���@��@��w@���@��@�
=@�5?@�z�@���@��@��@�;d@�X@�Z@�v�@�u@�@�@ߕ�@�S�@�"�@�{@�z�@�"�@��@��m@�
=@���@�A�@Ѻ^@� �@�;d@��@ͩ�@�%@�A�@˾w@ˍP@�l�@��H@�I�@�A�@�=q@å�@\@��h@���@�9X@��@���@���@��@��@���@�dZ@�v�@�/@��@���@��m@�;d@��@���@�M�@�@���@�7L@�bN@�b@��;@���@�t�@�|�@�;d@���@��#@�/@�Q�@�K�@��R@�X@���@�1@���@�t�@�33@�ff@���@�O�@�j@�1@���@��
@�K�@�
=@���@��+@�@��h@�/@�b@�C�@�ȴ@���@��\@�M�@��@�@�&�@�1'@�|�@�\)@�ƨ@�(�@�9X@�1@��m@��@�\)@�t�@�;d@��H@�=q@���@���@�p�@���@�bN@��@��F@�S�@�33@�"�@�
=@��@���@�ff@�M�@�$�@���@��#@���@�@���@��-@�hs@�`B@�O�@�?}@�V@��@�A�@��@��@���@��H@��y@��H@���@�M�@���@�@���@���@���@���@���@�@��@��@�V@��`@�r�@��@��F@�t�@�;d@�C�@�
=@��H@�v�@�V@�5?@�J@��@�?}@�?}@�7L@�?}@�&�@�&�@�&�@��@��`@��u@�Q�@��@���@�S�@�"�@�@���@�J@��h@���@��7@��7@��h@���@�G�@�%@���@��`@��/@���@��9@�I�@�9X@��
@��@�33@�@��H@���@��!@���@�ff@�$�@�@��-@�x�@�O�@�X@�x�@�`B@�V@��9@��@��@���@��
@�ƨ@��w@�ƨ@��w@��F@�t�@�C�@��@�
=@���@��@��!@�~�@�=q@��@���@�`B@�7L@�/@��G�O�@�]�@}�X@tM@i�@_y�@YA @R{�@I��@DtT@?@6��@1u�@+��@&�@#�@�X@(�@��@��@
�b@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�5B�/B�/B�/B�/B�/B�/B�/B�/B�/B�/B�/B�/B�)B�)B�/B�sB��B�B#�B(�B33B49B33B49B6FB5?B8RBC�BI�BcTBt�B~�B�%B�DB�VB�bB�{B�uB�uB�hB�\B�\B�DB�DB�7B�1B�%B�%B�B�B|�B{�By�Bu�BbNBZBF�B.B�BbBDBB��B��B��B�B�TB�B�B�wB�LB�-B��B�bBv�B`BB]/BP�B>wB,B�BuBhBJB
��B
�ZB
��B
�'B
��B
�bB
iyB
I�B
&�B
JB	��B	��B	�B	��B	��B	�DB	o�B	gmB	l�B	l�B	ffB	e`B	gmB	gmB	hsB	dZB	dZB	cTB	dZB	gmB	iyB	w�B	y�B	w�B	n�B	iyB	_;B	S�B	J�B	I�B	E�B	D�B	C�B	>wB	33B	9XB	P�B	P�B	N�B	F�B	@�B	9XB	49B	0!B	)�B	"�B	�B	{B	bB	JB		7B	%B	B��B��B��B�B�B�mB�ZB�TB�5B�
B��B��BɺBB�qB�dB�RB�?B�3B�B�B��B��B��B��B��B��B��B��B�oB�JB�1B�B�B� B{�By�Bw�Bq�Bp�Bl�BgmBffBdZBcTBbNBaHB]/B[#BZBW
BVBR�BQ�BM�BI�BH�BF�BG�BH�BH�BH�BF�BE�BE�BE�BE�BE�BC�BA�B?}B@�BA�B>wB:^B8RB8RB5?B49B5?B33B0!B1'B33B1'B0!B0!B/B.B.B.B,B+B-B,B,B-B.B.B/B49B6FB5?B6FB6FB6FB49B9XB=qB>wB?}B@�BB�BD�BF�BG�BH�BH�BH�BR�BT�BXBW
BZB\)B]/B_;B`BBaHBbNBdZBe`BhsBjBm�Bo�Bo�Bo�Bq�Br�Br�Br�Br�Bs�Br�Bs�Bu�Bv�Bv�Bw�Bw�Bw�Bw�By�B{�B}�B�B�B�+B�JB�VB�hB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�-B�3B�9B�?B�RB�qBBŢB��B��B�B�B�B�B�B�B�/B�TB�yB�B�B�B��B��B��B��B	B	B	B	B	B	1B	JB	bB	hB	uB	�B	�B	�B	�B	�B	 �B	 �B	!�B	#�B	&�B	)�B	+B	-B	/B	0!B	0!B	33B	5?B	9XB	?}B	?}B	A�B	B�B	B�B	C�B	C�B	D�B	D�B	H�B	N�B	Q�B	YB	\)B	\)B	aHB	bNB	e`B	iyB	k�B	m�B	o�B	o�B	p�B	q�B	w�B	{�B	�B	�B	�+B	�DB	�JB	�JB	�JB	�VB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�-B	�LB	�RB	�jB	�qB	�qB	�wB	�wB	�}B	�}B	��B	��B	ÖB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�)B	�/B	�5B	�BB	�HB	�HB	�HB	�NB	�HB	�XB	�B
?B
B
�B
%zB
,"B
5B
;B
@�B
H�B
M�B
S@B
V�B
\)B
b�B
gB
j�B
p�B
tnB
w2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�1B�+B�+B�+B�+B�+B�+B�+B�+B�+B�+B�+B�+B�%B�%B�+B�nB��B�B�B�B*)B+/B*)B+/B-<B,6B/IB:�B@�BZJBk�Bu�B}B�9B�KB�WB�pB�jB�kB�^B�RB�SB�;B�;B�/B)B}B}ByBw�Bs�Br�Bp�Bl�BYJBQB=�B%B�BdBGB�B��B��B��B�B�ZB�B�B��B�VB�7B��B�oBm�BWSBT@BG�B5�B#B�B
�BBaB
��B
�tB
��B
�EB
��B
��B
`�B
@�B
B
uB	�&B	��B	��B	�B	�B	�zB	f�B	^�B	c�B	c�B	]�B	\�B	^�B	^�B	_�B	[�B	[�B	Z�B	[�B	^�B	`�B	oB	qB	o	B	e�B	`�B	VwB	K5B	A�B	@�B	<�B	;�B	:�B	5�B	*sB	0�B	H#B	H#B	FB	=�B	7�B	0�B	+yB	'bB	!=B	B	�B	�B	�B	�B	 {B�jB�KB�9B�!B�B��B��B޴BۢBڜB�}B�SB�;B�#B�B��B��B��B��B��B�B�hB�[B�CB�>B�B�B�B��B��B��B��B��B�B|pBzcBwRBs9Bq-Bo"Bh�Bg�Bc�B^�B]�B[�BZ�BY�BX�BT�BRxBQsBN`BMZBJHBICBE*BAB@B> B?B@B@B@B> B<�B<�B<�B<�B<�B:�B8�B6�B7�B8�B5�B1�B/�B/�B,�B+�B,�B*�B'}B(�B*�B(�B'}B'}B&wB%qB%qB%qB#eB"_B$kB#eB#eB$lB%rB%rB&yB+�B-�B,�B-�B-�B-�B+�B0�B4�B5�B6�B7�B9�B;�B>B?B@B@B@BJOBL[BOmBNgBQzBS�BT�BV�BW�BX�BY�B[�B\�B_�Ba�Bd�Bf�Bf�Bf�BiBjBjBjBjBkBjBkBmBn%Bn%Bo+Bo+Bo+Bo+Bq7BsCBuPBxbB|zB~�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�%B�1B�=B�bB�uB��B��B��B��B��B��B��B��B��B��B�+B�IB�gB�gB�gB�nB�tB�tBԆBڪB��B��B��B��B�B�$B�0B�CB�aB�gB�nB�tB�tB��B	�B	�B	�B	
�B	�B	�B	�B	�B	B	B	B	B	*B	;B	!NB	"TB	$`B	&mB	'sB	'sB	*�B	,�B	0�B	6�B	6�B	8�B	9�B	9�B	:�B	:�B	;�B	;�B	@B	F)B	I<B	PfB	SxB	SxB	X�B	Y�B	\�B	`�B	b�B	d�B	f�B	f�B	g�B	h�B	oB	s4B	xSB	z_B	~xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�(B	�GB	�RB	�XB	�^B	�^B	�^B	�eB	�qB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�"B	�'B	�-B	�4B	�4B	�@B	�FB	�LB	�LB	�RB	�XB	�_B	�eB	�qB	�wB	�}B	׊B	؏B	؏B	؏B	ٕG�O�B	ߟB	�IB	��B
WB
�B
�B
#fB
,OB
2ZB
7�B
@B
D�B
J�B
NB
SlB
Y�B
^`B
b*B
g�B
k�B
nt111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.145 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144112022020411441120220204114411  AO  ARCAADJP                                                                    20200619170855    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170855  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170855  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114411  IP                  G�O�G�O�G�O�                