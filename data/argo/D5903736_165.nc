CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:54Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041154  20190604094024  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @����kB1   @��;��8@3,�C���d���$�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyZ�D���D�J�D���D�R�D��D�:�D��RD���D�HD�M�D���D�ؤD�
D�8�D�=D�ÅD�RD�K�D�k3D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
>@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�
=B�
=B��
B��
B��
B��
B��
B��
B��
B��
B�
=B��
B���B��
B��
B��
B��
B��
B��
B��
B�
=B�
=B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$t{D$�{D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+t{D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCt{DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt�GDyU�D��>D�HRD���D�PRD�)D�8RD���D��>D���D�K4D��HD��D�{D�6D��D���D��D�H�D�h�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�&�A�(�A�+A�-A�1'A�33A�33A�33A�33A�5?A�5?A�7LA�9XA�5?A�5?A�$�A� �A�"�A��A�%A�A���A��AجAؕ�AؑhAؕ�Aؙ�A؝�A؟�Aؙ�A�1'A׏\A֮A���A���A�Q�A̼jA��Aʇ+A���A�|�A���A�A��+A�ĜA���A���A�|�A�VA�(�A��A���A���A�t�A�bA�  A��A��!A�XA���A��/A� �A��!A��RA�
=A���A�A���A�/A��A�JA���A��A�K�A���A��`A�dZA�K�A���A��PA��RA��A��A�E�A��A���A�1'A�r�A�x�A�p�A�E�A�ZA���A�O�A���A�%A�  A��+A�oA�"�Al�A~�A}?}A{/Ay�TAx�/Aw�hAuArA�Ap$�Am�hAh��Adn�A`��A`{A\�+A[/AZn�AW�AU
=AT~�ASx�AQ��AQ�AO�hAMG�AKVAI�#AHQ�AF��AFz�AD(�ABffAA/A@�A?��A=��A<v�A;?}A8�RA7A6��A5��A3��A21'A1�A1�7A0��A0^5A-�A+�#A*ȴA)�7A(�A(�A'��A'+A&�+A%��A$v�A$ �A#dZA!��A jA�mA�A7LAȴAv�A�#A�/A��AoA�FA��AVA�A  AhsAĜA�A��AXA�+A&�A�A�DAK�A-A�PA
=A�A��A	��AM�A��A?}A�A�A�uAbA?}A��AffA�A �`@��@�ȴ@�M�@�`B@�ƨ@��`@�+@���@��@���@���@��@��@��H@�(�@�^5@�{@�E�@�n�@�{@�p�@�&�@�@���@��#@��u@��;@�|�@���@�5?@�b@�`B@�z�@�ƨ@�ȴ@�@Ӆ@Ұ!@ҸR@�o@���@�=q@��T@Ͳ-@ͩ�@̴9@��@�V@�V@�bN@���@��y@���@�?}@�&�@��@ě�@�ȴ@��h@���@� �@��;@���@�dZ@�33@���@�n�@��@��m@��@��@���@���@�n�@���@�hs@�X@�7L@�%@�(�@�o@���@�  @�=q@���@�&�@��@� �@�I�@�I�@�b@�1@��F@��
@�t�@�C�@��y@���@���@�b@�;d@�J@��@�E�@��\@��H@��\@��+@�n�@�J@��#@��T@��#@��@�/@�V@���@���@��@���@�v�@��@��P@�|�@�;d@��@�M�@�/@���@��D@��u@���@�9X@��w@�K�@�@��!@��^@��@���@��@��m@��@�|�@��y@��R@�ff@�=q@��@��@��#@��h@�O�@�&�@�Ĝ@�r�@�9X@��
@��@�"�@���@�E�@��@���@���@�@���@�p�@�7L@�/@��@�V@���@��j@�j@�A�@�b@�t�@��R@��!@�ȴ@��+@�^5@�M�@�{@��T@�x�@�G�@��@�%@���@��D@� �@�b@�  @��w@��@���@�=q@�%@��j@���@�bN@�b@��;@�bN@�j@�1'@��@�S�@���@��+@��\@��+@�~�@�E�@��@�x�@�?}@�%@��`@�Ĝ@�Ĝ@��@�j@�1'@�ƨ@��P@�C�@�;d@�;d@�;d@��@�@���@���@���@���@�V@�M�@�=q@��@�@�p�@�7L@�&�@�%@�r�@�Q�@�1'@��;@��w@���@�|�@�S�@�C�@�+@�o@�o@�o@�n�@�V@�E�@�5?@�J@��@��^@���@��h@�?}@��@�%@���@�(�@�b@��m@��F@��P@��@�K�@�33@�ff@��5@w��@o� @f�\@]�^@V�B@N��@G,�@>�M@90�@2ȴ@.O@(�|@$1@_p@@��@�@F@��@
_111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�&�A�(�A�+A�-A�1'A�33A�33A�33A�33A�5?A�5?A�7LA�9XA�5?A�5?A�$�A� �A�"�A��A�%A�A���A��AجAؕ�AؑhAؕ�Aؙ�A؝�A؟�Aؙ�A�1'A׏\A֮A���A���A�Q�A̼jA��Aʇ+A���A�|�A���A�A��+A�ĜA���A���A�|�A�VA�(�A��A���A���A�t�A�bA�  A��A��!A�XA���A��/A� �A��!A��RA�
=A���A�A���A�/A��A�JA���A��A�K�A���A��`A�dZA�K�A���A��PA��RA��A��A�E�A��A���A�1'A�r�A�x�A�p�A�E�A�ZA���A�O�A���A�%A�  A��+A�oA�"�Al�A~�A}?}A{/Ay�TAx�/Aw�hAuArA�Ap$�Am�hAh��Adn�A`��A`{A\�+A[/AZn�AW�AU
=AT~�ASx�AQ��AQ�AO�hAMG�AKVAI�#AHQ�AF��AFz�AD(�ABffAA/A@�A?��A=��A<v�A;?}A8�RA7A6��A5��A3��A21'A1�A1�7A0��A0^5A-�A+�#A*ȴA)�7A(�A(�A'��A'+A&�+A%��A$v�A$ �A#dZA!��A jA�mA�A7LAȴAv�A�#A�/A��AoA�FA��AVA�A  AhsAĜA�A��AXA�+A&�A�A�DAK�A-A�PA
=A�A��A	��AM�A��A?}A�A�A�uAbA?}A��AffA�A �`@��@�ȴ@�M�@�`B@�ƨ@��`@�+@���@��@���@���@��@��@��H@�(�@�^5@�{@�E�@�n�@�{@�p�@�&�@�@���@��#@��u@��;@�|�@���@�5?@�b@�`B@�z�@�ƨ@�ȴ@�@Ӆ@Ұ!@ҸR@�o@���@�=q@��T@Ͳ-@ͩ�@̴9@��@�V@�V@�bN@���@��y@���@�?}@�&�@��@ě�@�ȴ@��h@���@� �@��;@���@�dZ@�33@���@�n�@��@��m@��@��@���@���@�n�@���@�hs@�X@�7L@�%@�(�@�o@���@�  @�=q@���@�&�@��@� �@�I�@�I�@�b@�1@��F@��
@�t�@�C�@��y@���@���@�b@�;d@�J@��@�E�@��\@��H@��\@��+@�n�@�J@��#@��T@��#@��@�/@�V@���@���@��@���@�v�@��@��P@�|�@�;d@��@�M�@�/@���@��D@��u@���@�9X@��w@�K�@�@��!@��^@��@���@��@��m@��@�|�@��y@��R@�ff@�=q@��@��@��#@��h@�O�@�&�@�Ĝ@�r�@�9X@��
@��@�"�@���@�E�@��@���@���@�@���@�p�@�7L@�/@��@�V@���@��j@�j@�A�@�b@�t�@��R@��!@�ȴ@��+@�^5@�M�@�{@��T@�x�@�G�@��@�%@���@��D@� �@�b@�  @��w@��@���@�=q@�%@��j@���@�bN@�b@��;@�bN@�j@�1'@��@�S�@���@��+@��\@��+@�~�@�E�@��@�x�@�?}@�%@��`@�Ĝ@�Ĝ@��@�j@�1'@�ƨ@��P@�C�@�;d@�;d@�;d@��@�@���@���@���@���@�V@�M�@�=q@��@�@�p�@�7L@�&�@�%@�r�@�Q�@�1'@��;@��w@���@�|�@�S�@�C�@�+@�o@�o@�o@�n�@�V@�E�@�5?@�J@��@��^@���@��h@�?}@��@�%@���@�(�@�b@��m@��F@��P@��@�K�@�33G�O�@��5@w��@o� @f�\@]�^@V�B@N��@G,�@>�M@90�@2ȴ@.O@(�|@$1@_p@@��@�@F@��@
_111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBu�Bu�Bu�Bu�Bu�Bu�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bw�Bw�Bz�B{�B{�B}�B�B�B�B�1B�bB��B��B��B��B��B��BÖB?}BjBx�By�Bs�BhsBT�BH�BE�BD�BI�BgmB�B�\B��B��B��B�FB�}BĜB��B��BɺB��B��B��B�wB�3B�B��B��B�PB� B�1B�Bt�Bl�BhsBcTB]/BYBP�BC�B?}B5?B-B�B
=B�B�jB��B��B�PB{�Bp�BjBffB]/BO�B>wB,B�B	7B  B
��B
�sB
�B
��B
��B
��B
�uB
�PB
�B
q�B
gmB
_;B
S�B
A�B
+B
�B
B	�;B	B	��B	��B	�PB	�B	z�B	iyB	]/B	YB	Q�B	G�B	A�B	8RB	-B	!�B	�B	uB	JB	1B��B��B�B�B�yB�TB�5B�B��B��BɺBĜB��B�qB�dB�XB�LB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�PB�=B�1B�%B�B~�By�By�Bv�Bz�B{�By�Bx�Bz�Bx�Bv�By�By�Bw�Bx�By�By�By�Bw�Bv�Bv�Bu�Bw�By�Bx�Bw�Bu�Bq�Bo�Br�Bz�B|�B�=B�uB��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�'B�B�B�!B�-B�3B�?B�^B�^B�jB�qB�qB�wB�}B�}B��BÖBȴB��B��B�B�
B�
B�
B�BB�sB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	B	B	+B		7B	%B	B	%B	PB	\B	bB	uB	�B	�B	�B	�B	!�B	$�B	0!B	33B	49B	7LB	9XB	?}B	>wB	;dB	<jB	?}B	E�B	M�B	N�B	N�B	N�B	O�B	S�B	T�B	VB	XB	ZB	ZB	[#B	\)B	[#B	^5B	^5B	bNB	iyB	jB	l�B	o�B	p�B	r�B	r�B	s�B	u�B	y�B	}�B	� B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�PB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�LB	�RB	�XB	�^B	�jB	�jB	�qB	�wB	�wB	��B	ÖB	ĜB	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�)B	�/B	�;B	�HB	�HB	�HB	�NB	�ZB	�`B	�`B	�TB	�ZB	�fB	�`B	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
	7B

=B
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
VB
VB
\B
\B
bB
bB
bB
bB
hB
hB
'�B
�B
�B
'�B
1�B
:B
@4B
G�B
NB
UMB
Z�B
_;B
a�B
fLB
j�B
o B
qvB
v`B
z�B
~]B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bq�Bq�Bq�Bq�Bq�Bq�Br�Br�Br�Br�Br�Br�Br�Bs�Bs�Bv�Bw�Bw�By�B|�B|�B~�B�B�8B�^B�yB��B��B��B��B�lB;QBfTBt�Bu�Bo�BdBBP�BD�BAmB@pBE�Bc@B�B�,B��B��B��B�B�PB�mB�[B�TBŉB��B��BʰB�OB�B��B��B�ZB�&B{�B�B}�Bp�BhcBdNB_0BYBT�BL�B?pB;VB1B(�BvBB�nB�FB��B�cB�*Bw�Bl�Bf\BbCBYBK�B:ZB'�B�BB
��B
�B
�VB
��B
�fB
��B
��B
�`B
�5B
|�B
m�B
cWB
['B
O�B
=wB
&�B
~B	� B	�'B	��B	��B	��B	�CB	}�B	v�B	emB	Y#B	U
B	M�B	C�B	=~B	4KB	)B	�B	�B	lB	?B	,B��B��B��B�B�sB�KB�-B�B��B��BŴB��B��B�jB�bB�SB�HB�,B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�zB�pB�`B�NB�;B�1B�$BBz�Bu�Bu�Br�Bv�Bw�Bu�Bt�Bv�Bt�Br�Bu�Bu�Bs�Bt�Bu�Bu�Bu�Bs�Br�Br�Bq�Bs�Bu�Bt�Bs�Bq�Bm�Bk�Bn�Bv�Bx�B�=B�tB��B��B��B��B��B��B��B��B��B��B��B�B�B�)B�(B�B�B�#B�+B�3B�?B�]B�^B�jB�rB�rB�xB�|B�|B��B��BĵB��B��B�B�	B�	B�	B�AB�pB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�
B�B�B	 B	 B	(B	8B	&B�B	&B		QB	YB	cB	vB	�B	�B	�B	�B	�B	 �B	,B	/.B	05B	3JB	5QB	;xB	:tB	7aB	8eB	;zB	A�B	I�B	J�B	J�B	J�B	K�B	O�B	P�B	RB	TB	VB	VB	WB	X%B	W$B	Z5B	Z2B	^KB	euB	f{B	h�B	k�B	l�B	n�B	n�B	o�B	q�B	u�B	y�B	{�B	~B	�B	�B	�B	�B	�&B	�1B	�;B	�IB	�\B	�qB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�7B	�DB	�NB	�SB	�YB	�aB	�cB	�jB	�qB	�sB	�B	��B	��B	��B	��B	ãB	ĭB	��B	��B	��B	��B	��B	�B	�B	� B	�$B	�#B	�'B	�4B	�BB	�BB	�?B	�MB	�XB	�ZB	�ZB	�KB	�VB	�]B	�ZB	�YB	�^B	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�
B
 B
 B
B
B
B
 B
#B
$B
%B
"B
#B
/B
5B
;B
>B
>B
@B
AB
CB
CB
AB
	HB
	IB

NB

PB
WB
WB
WB
ZB
YB
ZB
aB
^G�O�B
�B
�B
#�B
-�B
6B
<,B
C�B
I�B
QFB
V�B
[,B
]�B
bHB
f�B
j�B
mlB
rVB
v�B
zTB
}�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.004(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940242019060409402420190604094024  AO  ARCAADJP                                                                    20181121041154    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041154  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041154  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094024  IP                  G�O�G�O�G�O�                