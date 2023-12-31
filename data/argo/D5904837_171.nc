CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-07-17T17:01:50Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210717170150  20220110114555  5904837 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  6694                            2C  D   APEX                            7686                            030716                          846 @ل����1   @ل�ww���C�`A�7L@D�x���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C#�fC&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cw�fCy�fC|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0y�D0��D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DYy�DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� DefDe�fDf  Df� Dg  Dg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D� D�I�D�� D��fD�fD�I�D�l�D�ɚD���D�\�D���D�� D���D�I�Dړ3D���D� D�L�D� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
>@�p�@�p�A�RA=�A^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
B��
C�C�C�C�C	�C�C�C�C�C�C�C�CC�C�C�C!�C#��C%�C(C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM��CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�CfCg�Ci�Ck�Cm�Co��Cq�Cs�Cu�Cw��Cy��C{�C}�C�C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D�{Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0t{D0�{D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGt{DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DN�GDN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYt{DY��DZz�DZ��D[z�D[��D\z�D\��D]t{D]��D^z�D^��D_z�D_��D`z�D`�{Daz�Da��Dbz�Db��Dcz�Dc��Ddz�DeGDe�GDe��Dfz�Df��Dgz�Dg��Dhz�DiGDiz�Di��Djz�Dj��Dkz�Dk��Dl�GDl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Dy�{D�qD�GD�}qD���D��D�GD�j>D��D��>D�Z>D��>DǽqD��D�GDڐ�D��>D�qD�J>D�}qD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A��9A��FA��FA��FA��-A��!A��FA��RA��RA��RA��FA��FA��FA���A���A���A���A���A���A��-A���A���A���A���A�hsA�E�A�A�A�+A��A��A�oA�bA�bA�bA�bA�bA�JA�A�  A���A��mA��A��jA���A��A���A���A���A���A���A��PA�^5A�oA��
A��-A���A��A�VA� �A��A���A���A��A��uA�x�A�A�A��A�ffA�{A��wA���A�n�A�7LA�VA���A��jA�S�A�  A��^A���A�C�A���A��A�t�A�p�A�t�A�~�A��^A��wA���A�ĜA�ƨA���A�/A��A���A��A�A�%A�%A��mA���A��hA��7A�jA��7A��A�1'A�bA�
=A��A��jA��DA�G�A�{A��A/A~�yA~�\A~{A}7LA|ĜA|JA{+Az�jAzbNAy�TAy7LAx�DAw�mAwS�Av��Au��At��At-As�Ar~�Aq�#Aq�wAqXAo�#An~�Am&�Al�uAlI�AlJAkAk�PAkhsAkS�Aj�9AjAihsAi
=AhZAgC�Af�+Ae�mAd�yAd�AcXAb��AbQ�Ab1Aa�FAa;dA`��A`^5A`1'A`A_�mA_�A_G�A^��A^��A^A�A]hsA\��A[�A[`BA[�AZ��AZQ�AY�AYO�AX�AX��AXjAX=qAW��AV�yAVQ�AV�AUx�AT�DAS��ASO�AS;dAS�AR��AR=qAQK�AQ7LAP�APAO�wAO��AO�AOXAOC�AO?}AO7LAOoAN-AM�FAM�AMO�AM�AL^5ALAK`BAJȴAJn�AI��AH��AH�!AH�AH��AHJAG�wAG"�AGAF�\AFE�AE��AD^5AC��AC��ACC�AB��AB-AA�A@��A@5?A?�wA>�A=�A=A<9XA;�
A;33A:��A:  A9�A8�\A8$�A8A7�#A7�-A7x�A7oA5�TA4~�A4  A3�
A3��A3XA3;dA3A2jA1��A1dZA1�A0�HA0��A0��A0�9A0�+A/�A/�-A/�A.��A.E�A-��A-+A,jA,1'A+�A+�hA+7LA*�\A*VA)A)\)A)33A(�jA(ZA'�;A'�A&ȴA&�uA&(�A&JA%��A%��A%33A$�\A#��A#��A#�PA#x�A#�A"�/A"��A"-A!�A!�A �RA�TA�A&�AbNAK�A��AM�AA?}A��AE�A�#AC�A��AjA��A�hA/A"�A%A�A�9AA�AA�mAƨAx�A;dA��AffAM�A�TA��A��An�A�^AM�A��A�;A�A�yA�9A��An�AQ�A$�A��AdZA/AVA��A�FA�A
v�A
A�A
JA	�A��A��A%Az�A��Al�A��A1'A�7A�/A�#A ��@��@�~�@��@���@���@��@�l�@�o@�"�@���@�$�@��@��@��`@���@��@�  @���@�O�@�?}@��@� �@��y@�hs@���@� �@��@�!@�h@� �@�o@�7L@�dZ@�ȴ@�M�@��@�?}@��@�A�@�\)@�ff@���@��@�bN@���@ߍP@�M�@ܛ�@ܓu@܃@�Q�@�33@�O�@��@��@׶F@և+@ՙ�@��
@�j@ΰ!@��@�o@�Ĝ@� �@ǝ�@�|�@Ǖ�@�S�@�33@��@�$�@�p�@�hs@�j@�ƨ@�
=@�@��@�C�@��@�J@��u@��P@��R@��+@�M�@�J@�Ĝ@��\@���@�7L@��@��@�z�@��@�@�
=@�7L@�x�@�v�@��H@w��@q%@j��@c�m@X  @RJ@MO�@HĜ@D��@>�+@;o@7�@6{@2�\@1&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A���A���A���A��9A��FA��FA��FA��-A��!A��FA��RA��RA��RA��FA��FA��FA���A���A���A���A���A���A��-A���A���A���A���A�hsA�E�A�A�A�+A��A��A�oA�bA�bA�bA�bA�bA�JA�A�  A���A��mA��A��jA���A��A���A���A���A���A���A��PA�^5A�oA��
A��-A���A��A�VA� �A��A���A���A��A��uA�x�A�A�A��A�ffA�{A��wA���A�n�A�7LA�VA���A��jA�S�A�  A��^A���A�C�A���A��A�t�A�p�A�t�A�~�A��^A��wA���A�ĜA�ƨA���A�/A��A���A��A�A�%A�%A��mA���A��hA��7A�jA��7A��A�1'A�bA�
=A��A��jA��DA�G�A�{A��A/A~�yA~�\A~{A}7LA|ĜA|JA{+Az�jAzbNAy�TAy7LAx�DAw�mAwS�Av��Au��At��At-As�Ar~�Aq�#Aq�wAqXAo�#An~�Am&�Al�uAlI�AlJAkAk�PAkhsAkS�Aj�9AjAihsAi
=AhZAgC�Af�+Ae�mAd�yAd�AcXAb��AbQ�Ab1Aa�FAa;dA`��A`^5A`1'A`A_�mA_�A_G�A^��A^��A^A�A]hsA\��A[�A[`BA[�AZ��AZQ�AY�AYO�AX�AX��AXjAX=qAW��AV�yAVQ�AV�AUx�AT�DAS��ASO�AS;dAS�AR��AR=qAQK�AQ7LAP�APAO�wAO��AO�AOXAOC�AO?}AO7LAOoAN-AM�FAM�AMO�AM�AL^5ALAK`BAJȴAJn�AI��AH��AH�!AH�AH��AHJAG�wAG"�AGAF�\AFE�AE��AD^5AC��AC��ACC�AB��AB-AA�A@��A@5?A?�wA>�A=�A=A<9XA;�
A;33A:��A:  A9�A8�\A8$�A8A7�#A7�-A7x�A7oA5�TA4~�A4  A3�
A3��A3XA3;dA3A2jA1��A1dZA1�A0�HA0��A0��A0�9A0�+A/�A/�-A/�A.��A.E�A-��A-+A,jA,1'A+�A+�hA+7LA*�\A*VA)A)\)A)33A(�jA(ZA'�;A'�A&ȴA&�uA&(�A&JA%��A%��A%33A$�\A#��A#��A#�PA#x�A#�A"�/A"��A"-A!�A!�A �RA�TA�A&�AbNAK�A��AM�AA?}A��AE�A�#AC�A��AjA��A�hA/A"�A%A�A�9AA�AA�mAƨAx�A;dA��AffAM�A�TA��A��An�A�^AM�A��A�;A�A�yA�9A��An�AQ�A$�A��AdZA/AVA��A�FA�A
v�A
A�A
JA	�A��A��A%Az�A��Al�A��A1'A�7A�/A�#A ��@��@�~�@��@���@���@��@�l�@�o@�"�@���@�$�@��@��@��`@���@��@�  @���@�O�@�?}@��@� �@��y@�hs@���@� �@��@�!@�h@� �@�o@�7L@�dZ@�ȴ@�M�@��@�?}@��@�A�@�\)@�ff@���@��@�bN@���@ߍP@�M�@ܛ�@ܓu@܃@�Q�@�33@�O�@��@��@׶F@և+@ՙ�@��
@�j@ΰ!@��@�o@�Ĝ@� �@ǝ�@�|�@Ǖ�@�S�@�33@��@�$�@�p�@�hs@�j@�ƨ@�
=@�@��@�C�@��@�J@��u@��P@��R@��+@�M�@�J@�Ĝ@��\@���@�7L@��@��@�z�G�O�@�@�
=@�7L@�x�@�v�@��H@w��@q%@j��@c�m@X  @RJ@MO�@HĜ@D��@>�+@;o@7�@6{@2�\@1&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bw�Bx�Bw�Bx�Bw�Bw�Bw�Bw�Bv�Bu�Bv�Bu�Bu�Bu�Bv�Bu�Bu�Bu�Bv�Bt�Bn�Bn�Bl�Bk�Bk�Bk�Bk�Bk�Bk�Bk�BiyBjBk�BjBjBiyBjBiyBjBk�Bk�Bk�BjBjBjBjBiyBhsBdZBcTBbNBaHB_;B]/B\)BZBYBYBXBVBVBR�BQ�BT�BS�BS�BT�BS�BR�BS�BM�BF�B?}B7LB2-B(�B�BBBBBB{B�B$�B(�B.B5?B2-B2-B2-B/B0!B2-B33B5?B7LB<jB?}BB�BN�BN�BH�BC�BD�BB�B=qB9XB1'B+B(�B"�B�B�B�BbBJB
=BB��B��B��B�B�B�yB�ZB�5B�)B��B��B��BȴB��B�}B�}B�RB�B��B��B��B��B��B��B��B��B��B�\B�JB�=B�B�B{�Bx�Bs�Bn�BhsBdZBbNBaHB`BB_;B\)BZBYBXBW
BT�BS�BM�BM�BK�BG�B@�B=qB8RB6FB33B0!B/B)�B'�B#�B"�B �B�B�B\BJB1BB��B��B�B�B�B�B�fB�ZB�ZB�5B�#B�B�B�B�
B�
B�B��B��BɺBǮBŢBÖB�wB�XB�3B�B��B��B��B��B��B��B��B�hB�PB�DB�%B�B|�Bq�Bk�BiyBe`B^5BW
BP�BF�B@�B<jB6FB+B"�B�B�B{B\B
=BB��B��B��B��B��B�B�B�`B��B��B��BɺBŢBĜBB�wB�RB�?B�3B�-B�9B�LB�LB�FB�9B�!B�B��B��B��B��B��B�{B�oB�bB�VB�=B�+B�B~�B|�Bz�Bv�Bs�Bl�BjBhsBffBe`BcTBaHB^5B[#BW
BS�BR�BQ�BO�BL�BK�BF�BD�BB�B;dB5?B2-B.B(�B�B�B�BoBVB	7B%BB
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�sB
�mB
�mB
�`B
�TB
�HB
�5B
�5B
�#B
�B
��B
��B
ȴB
�wB
�^B
�qB
�qB
�jB
�^B
�^B
�XB
�RB
�LB
�FB
�-B
�'B
�!B
�B
��B
��B
��B
��B
��B
��B
�{B
�\B
�1B
�B
~�B
{�B
y�B
t�B
n�B
iyB
cTB
ZB
R�B
N�B
I�B
R�B
VB
P�B
Q�B
P�B
Q�B
Q�B
P�B
S�B
Q�B
Q�B
P�B
O�B
N�B
L�B
G�B
G�B
F�B
D�B
@�B
:^B
9XB
7LB
6FB
49B
1'B
.B
+B
&�B
 �B
�B
�B
�B
�B
�B
�B
�B
oB
bB
VB
DB
DB
	7B
1B
B
B
%B
%B
B
B	��B	��B	��B	��B	�B	�B	�#B	��B	ȴB	ĜB	�dB	�^B	�RB	�LB	�LB	�LB	�FB	�9B	�3B	�'B	�LB	�?B	�9B	�3B	�'B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�bB	�\B	�\B	�\B	�\B	�\B	�1B	��B	��B	��B	��B	�
B	�B	��B
B
�B
5?B
M�B
_;B
p�B
�B
��B
�?B
��B
�B
�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bw�Bx�Bw�Bx�Bw�Bw�Bw�Bw�Bv�Bu�Bv�Bu�Bu�Bu�Bv�Bu�Bu�Bu�Bv�Bt�Bn�Bn�Bl�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bi�Bj�Bk�Bj�Bj�Bi�Bj�Bi�Bj�Bk�Bk�Bk�Bj�Bj�Bj�Bj�Bi�Bh{BdbBc\BbVBaPB_CB]7B\1BZ%BYBYBXBVBVBR�BQ�BUBT BT BUBT BR�BT BM�BF�B?�B7TB25B(�B�BBBBB!B�B�B$�B(�B.B5GB25B25B25B/#B0)B25B3;B5GB7TB<rB?�BB�BN�BN�BH�BC�BD�BB�B=yB9`B1/B+
B(�B"�B�B�B�BjBRB
EBB��B��B��B�B�B�B�bB�=B�1B��B��B��BȼB��B��B��B�ZB�#B��B��B��B��B��B��B��B��B��B�eB�SB�FB�(B�B{�Bx�Bs�Bn�Bh|BdcBbWBaQB`KB_DB\2BZ&BY BXBWBUBTBM�BM�BK�BG�B@�B=zB8[B6OB3<B0*B/$B*B'�B#�B"�B �B�B�BeBSB:BB��B��B�B�B�B�B�oB�cB�cB�>B�,B�&B� B�B�B�B�B�B��B��BǷBūBßB��B�aB�<B�B�B��B��B��B��B��B��B�qB�YB�MB�.B�B|�Bq�Bk�Bi�BeiB^>BWBP�BF�B@�B<sB6OB+B"�B�B�B�BeB
FB"B�B��B��B��B��B�B�B�iB�B��B��B��BūBĥBB��B�[B�HB�<B�6B�BB�UB�UB�OB�BB�*B�B��B��B��B��B��B��B�xB�kB�_B�FB�4B�BB|�Bz�Bv�Bs�Bl�Bj�Bh|BfoBeiBc]BaQB^>B[,BWBTBR�BQ�BO�BL�BK�BF�BD�BB�B;mB5HB26B.B(�B�B�B�BxB_B	@B.BB
�B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�|B
�vB
�vB
�iB
�]B
�QB
�>B
�>B
�,B
�B
��B
��B
ȽB
��B
�gB
�zB
�zB
�sB
�gB
�gB
�aB
�[B
�UB
�OB
�6B
�0B
�*B
�B
��B
��B
��B
��B
��B
��B
��B
�fB
�;B
�#B
B
{�B
y�B
t�B
n�B
i�B
c^B
Z'B
R�B
N�B
I�B
R�B
VB
P�B
Q�B
P�B
Q�B
Q�B
P�B
TB
Q�B
Q�B
P�B
O�B
N�B
L�B
G�B
G�B
F�B
D�B
@�B
:hB
9bB
7VB
6PB
4CB
11B
.B
+B
&�B
 �B
�B
�B
�B
�B
�B
�B
�B
yB
lB
`B
NB
NB
	AB
;B
)B
)B
/B
/B
)B
B	��B	��B	��B	��B	�B	�B	�-B	��B	ȾB	ĦB	�nB	�hB	�\B	�VB	�VB	�VB	�PB	�CB	�=B	�1B	�VB	�IB	�CB	�=B	�1B	�B	��B	� B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�lB	�fB	�fB	�fB	�fG�O�B	�;B	��B	��B	� B	��B	�B	�B	��B
B
�B
5IB
M�B
_EB
p�B
�B
��B
�IB
��B
�!B
�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             202201101145552022011011455520220110114555  AO  ARCAADJP                                                                    20210717170150    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210717170150  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210717170150  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220110114555  IP                  G�O�G�O�G�O�                