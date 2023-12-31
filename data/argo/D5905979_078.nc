CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:12Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170912  20220204114418  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               NA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؿ��fյ1   @ؿ�o�y@7-�����c��t�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    NA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  AᙚA�33A�33B  B  B  B ffB'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/y�D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�fD���D�Y�D���D���D��D�T{D���D��D�HD�Q�D���DǠRD��D�]Dڕ�D��D�
D�K�D��D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A���A�\A��\B�B�B�B zB'G�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
B�
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
B��
B��
B��
B��
B��
B�
=B��
B���B��
C�C�C�C�C	�C�C�C�C��C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C.C0C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�CrCs�Cu�Cw�Cy�C{�C}�C�C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D�{D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,�GD,��D-z�D-��D.z�D.��D/t{D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF�{DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN�{DOz�DO��DPz�DP��DQz�DQ�{DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�DtǮDy�GD��]D�WD�� D��gD�]D�Q�D��D�ʐD��D�O]D���Dǝ�D�]D�Z�Dړ4D��D�{D�IHD�HD�њ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AˁAˇ+Aˇ+Aˇ+A˅A˅AˁA�|�A�~�AˁA�~�A�x�A�v�A�ffA�^5A��
A�$�A�ȴA�7LA�$�A�%A�=qA�r�A�dZA�%A�x�A���A��uA��
A���A��A�x�A�;dA�ffA���A��A��A�ĜA��/A���A���A��A���A���A��yA��A��A��\A�E�A��jA��yA�A��yA�{A���A�x�A��`A��7A��A�/A��A���A�-A���A���A��uA��/A���A�/A��HA��FA�bA�  A��PA�A�M�A���A��uA��A�`BA��A�I�A��HA��A�dZA���A��/A�|�A�\)A���A�C�A�x�A�Q�A���A���A�ZA�bNA��DA���A�l�A���A�(�A��FA��TA�|�A���A~�!A|ZA{��AzI�Av�!As`BAr��Aq"�An{Alz�Aj��AiO�Ag�hAfĜAd(�Ab�yAb�+Aa�A_�#A^ZA]
=A\bAZM�AY�PAW
=AUXAT�ASAS�^AS/AQ�TANȴAM��AL(�AJ��AIO�AHĜAH��AG�#AGK�AG�AF�DAD9XAC�FAB��AB��AB�AA��A??}A>9XA;+A8I�A7��A7XA5x�A4�DA3��A2�yA2�HA3;dA3;dA2jA2�A1�-A1C�A0^5A/��A/��A/��A/;dA.1A-�A,��A,bNA,{A+�A*�HA*�A)ƨA(��A($�A'�TA'33A&jA&1A$M�A"��A!�#A!\)A!33A -A�A�RA+AjA��A�+A
=AƨA~�AƨA�;A��AƨA��A�A�\AO�A	��AȴA�A��A�A��A��A�A �9A n�A -@�5?@�A�@��@��@�M�@�-@�A�@�/@��/@��@��y@�7@�b@�Ĝ@�S�@���@���@��;@��y@�E�@�^@�?}@��@�~�@�X@ߍP@�=q@�?}@ܣ�@���@��@׶F@ա�@�9X@��@҇+@���@��@��#@���@љ�@ёh@�hs@�&�@�%@�l�@�\)@��@�7L@�Q�@��@�l�@Χ�@�hs@���@�1'@� �@��;@�~�@�Ĝ@�I�@��@���@�^5@���@��@�j@�@�r�@��+@���@��/@���@�v�@�X@���@�/@��@��9@���@���@���@���@��-@��`@��H@���@�`B@�bN@��m@��h@���@�Q�@��@���@���@���@�Ĝ@��9@�j@��@�`B@�ff@�{@�~�@�`B@�Ĝ@�b@��+@��h@��@��u@�@��-@�1@���@���@��@��@��@��j@���@�V@�?}@�?}@��@���@��9@��@�Z@�9X@�1@�dZ@���@��@���@��@�J@��@�?}@�7L@�/@��`@��@��
@���@�t�@�K�@�ȴ@�5?@��@�G�@�Ĝ@���@�(�@���@�K�@�o@��@�V@���@��@��@��#@�O�@���@�Ĝ@��u@�j@�I�@� �@���@��@�C�@�+@�
=@���@�^5@��@���@�hs@��/@�r�@��@�|�@�S�@��@���@�=q@��@�@��@��@�~�@�~�@�E�@�{@��#@�O�@�%@���@�j@�A�@��m@�t�@�+@�
=@���@�J@��@��#@���@��^@�X@�%@�%@��j@���@�z�@�1'@���@��w@�t�@��@�o@�o@�o@��@���@�ff@�M�@�^5@�^5@�M�@��@���@���@��@�?}@��j@��D@��@���@��@�bN@�1'@�1@�  @�  @��@��;@��w@���@��P@�t�@�K�@�+@�@��y@��@��R@�v�@�ff@�E�@�
=@{�@u��@j��@_a@W�
@Q2a@L(�@Fd�@@�j@9�3@6�@/�V@*��@%Vm@��@�@�9@bN@�@خ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AˁAˇ+Aˇ+Aˇ+A˅A˅AˁA�|�A�~�AˁA�~�A�x�A�v�A�ffA�^5A��
A�$�A�ȴA�7LA�$�A�%A�=qA�r�A�dZA�%A�x�A���A��uA��
A���A��A�x�A�;dA�ffA���A��A��A�ĜA��/A���A���A��A���A���A��yA��A��A��\A�E�A��jA��yA�A��yA�{A���A�x�A��`A��7A��A�/A��A���A�-A���A���A��uA��/A���A�/A��HA��FA�bA�  A��PA�A�M�A���A��uA��A�`BA��A�I�A��HA��A�dZA���A��/A�|�A�\)A���A�C�A�x�A�Q�A���A���A�ZA�bNA��DA���A�l�A���A�(�A��FA��TA�|�A���A~�!A|ZA{��AzI�Av�!As`BAr��Aq"�An{Alz�Aj��AiO�Ag�hAfĜAd(�Ab�yAb�+Aa�A_�#A^ZA]
=A\bAZM�AY�PAW
=AUXAT�ASAS�^AS/AQ�TANȴAM��AL(�AJ��AIO�AHĜAH��AG�#AGK�AG�AF�DAD9XAC�FAB��AB��AB�AA��A??}A>9XA;+A8I�A7��A7XA5x�A4�DA3��A2�yA2�HA3;dA3;dA2jA2�A1�-A1C�A0^5A/��A/��A/��A/;dA.1A-�A,��A,bNA,{A+�A*�HA*�A)ƨA(��A($�A'�TA'33A&jA&1A$M�A"��A!�#A!\)A!33A -A�A�RA+AjA��A�+A
=AƨA~�AƨA�;A��AƨA��A�A�\AO�A	��AȴA�A��A�A��A��A�A �9A n�A -@�5?@�A�@��@��@�M�@�-@�A�@�/@��/@��@��y@�7@�b@�Ĝ@�S�@���@���@��;@��y@�E�@�^@�?}@��@�~�@�X@ߍP@�=q@�?}@ܣ�@���@��@׶F@ա�@�9X@��@҇+@���@��@��#@���@љ�@ёh@�hs@�&�@�%@�l�@�\)@��@�7L@�Q�@��@�l�@Χ�@�hs@���@�1'@� �@��;@�~�@�Ĝ@�I�@��@���@�^5@���@��@�j@�@�r�@��+@���@��/@���@�v�@�X@���@�/@��@��9@���@���@���@���@��-@��`@��H@���@�`B@�bN@��m@��h@���@�Q�@��@���@���@���@�Ĝ@��9@�j@��@�`B@�ff@�{@�~�@�`B@�Ĝ@�b@��+@��h@��@��u@�@��-@�1@���@���@��@��@��@��j@���@�V@�?}@�?}@��@���@��9@��@�Z@�9X@�1@�dZ@���@��@���@��@�J@��@�?}@�7L@�/@��`@��@��
@���@�t�@�K�@�ȴ@�5?@��@�G�@�Ĝ@���@�(�@���@�K�@�o@��@�V@���@��@��@��#@�O�@���@�Ĝ@��u@�j@�I�@� �@���@��@�C�@�+@�
=@���@�^5@��@���@�hs@��/@�r�@��@�|�@�S�@��@���@�=q@��@�@��@��@�~�@�~�@�E�@�{@��#@�O�@�%@���@�j@�A�@��m@�t�@�+@�
=@���@�J@��@��#@���@��^@�X@�%@�%@��j@���@�z�@�1'@���@��w@�t�@��@�o@�o@�o@��@���@�ff@�M�@�^5@�^5@�M�@��@���@���@��@�?}@��j@��D@��@���@��@�bN@�1'@�1@�  @�  @��@��;@��w@���@��P@�t�@�K�@�+@�@��y@��@��R@�v�@�ffG�O�@�
=@{�@u��@j��@_a@W�
@Q2a@L(�@Fd�@@�j@9�3@6�@/�V@*��@%Vm@��@�@�9@bN@�@خ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB1'B0!B0!B0!B0!B0!B1'B1'B1'B1'B1'B2-B2-B33BR�BŢB�
B��B(�BA�BXBYBE�B6FB2-B:^B=qBA�BI�BO�BYBhsBz�B�7B�B�`BÖB��By�Be`Bm�Bu�BdZBD�B`BBv�Bq�Bv�B�+Bt�B�7B�uB��B��B��B�B�B�!B�dB�9B�B��B��B�\B�DB�Bv�BdZB]/BW
BG�B6FB49B1'B�BuBPB1BB�B�`B�/B�
B��B�wB�?B��B�B[#BP�B6FB$�B�B+B
��B
�B
�fB
�B
�dB
��B
��B
�bB
|�B
iyB
dZB
J�B
7LB
&�B
�B
oB
B	�HB	�B	��B	��B	�9B	��B	��B	�uB	�\B	�B	u�B	r�B	q�B	ffB	^5B	S�B	Q�B	E�B	>wB	2-B	%�B	�B	 �B	!�B	�B	�B	B��B��B��B�mB�mB�B�B�B��B��B�yB�mB�yB�B�B��B�B�TB��B�FB�qBB�RB�B��B��B��B�B�}B�qB�wB��B��B��B�qB�jB�dB�^B�LB�3B�'B�B�B�B��B��B��B��B��B��B��B��B�{B�uB�=B�+B�B�B�B|�B|�Bw�Bu�Bs�Bq�Bk�BjBhsBe`B_;BZBS�BQ�BN�BL�BI�BH�BH�BF�BE�BF�BD�BD�BD�BB�BA�BA�BB�BA�B?}B?}B?}B>wBA�B>wB>wB=qB>wB=qB>wB>wB>wB>wB>wB?}B?}B?}B?}B?}BA�B@�BA�BB�BC�BG�BH�BH�BG�BI�BL�BQ�BO�BO�BO�BP�BS�BYBZBYBYB\)B`BBy�B~�B� B�B�1B�1B�=B�DB�JB�JB�=B�=B�DB�DB�hB�hB�oB�{B�uB�bB�oB��B��B��B��B��B��B��B��B��B��B��B�'BĜB��B��B�)B�5B�HB�ZB�TB�#B��BÖBÖB��B�B�B�B�B�B�B�B�BB�NB�HB�B��B��B��B	{B	�B	�B	�B	�B	�B	�B	$�B	'�B	&�B	$�B	$�B	#�B	#�B	$�B	'�B	+B	0!B	6FB	9XB	;dB	=qB	>wB	?}B	B�B	B�B	B�B	D�B	G�B	I�B	H�B	I�B	?}B	=qB	>wB	?}B	A�B	D�B	E�B	F�B	F�B	F�B	G�B	I�B	J�B	J�B	L�B	O�B	P�B	R�B	T�B	W
B	XB	YB	]/B	`BB	aHB	bNB	dZB	jB	n�B	o�B	q�B	s�B	u�B	v�B	x�B	z�B	{�B	|�B	~�B	�B	�B	�B	�B	�B	�+B	�=B	�PB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�?B	�?B	�FB	�FB	�FB	�LB	�XB	�^B	�dB	�dB	�jB	�qB	�qB	�qB	�qB	�}B	�}B	�}B	�}B	��B	��B	��B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ŢB	ƨB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	��B	��B
�B
�B
 'B
($B
1'B
4�B
:�B
@4B
G�B
J�B
Q�B
V�B
ZkB
_�B
e�B
kB
oB
t�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B)�B(�B(�B(�B(�B(�B)�B)�B)�B)�B)�B*�B*�B+�BKOB��B�`B�NB!HB9�BP`BQgB=�B.�B*�B2�B5�B9�BBBH1BQhB`�Bs0B��B�`BݩB��B��Br+B]�Be�BnB\�B<�BX�BoBi�BoB{BmB��B��B��B�DB�7B�PB�iB�oB��B��B�VB��B��B��B��B{cBoB\�BU�BO^B@B.�B,�B)~BB�B�B �B�`B��BݼBՌB�gB�+B��B��B�=BzoBS�BIKB.�BFB�B
��B
�MB
�B
��B
�rB
��B
�IB
�B
��B
ucB
a�B
\�B
C9B
/�B
cB
-B

�B	��B	��B	ҝB	�rB	�B	��B	�[B	�0B	��B	��B	y�B	nJB	k7B	j1B	^�B	V�B	L�B	JvB	>,B	7B	*�B	pB	EB	RB	XB	KB	!B��B��B�wB�YB��B��B�)B�)B�"B�fB�wB�B��B�B�6B�GB�`B�B��B�sB��B�B�$B��B��B��B��B��B��B�B�B�B�B�B�B�B� B��B��B��B��B��B��B��B��B��B��B�uB�dB�QB�EB�?B�&B�B�B��B�B}�Bz�Bz�Bu�Bu�BpkBn_BlSBjGBd"BcBaB]�BW�BR�BL�BJ�BGyBEmBB[BAUBAUB?IB>CB?JB=>B=>B=>B;1B:+B:+B;2B:,B8 B8 B8 B7B:,B7B7B6B7B6B7B7B7B7B7B8!B8!B8!B8!B8!B:-B9'B:-B;3B<:B@RBAXBAXB@RBB^BEqBJ�BH�BH�BH�BI�BL�BQ�BR�BQ�BQ�BT�BX�Br}Bw�Bx�B|�B��B��B��B��B��B��B��B��B��B��B�	B�	B�B�B�B�B�B�YB�YB�SB�GB�;B�AB��B��B�lB�rB�~B��B�;B�(B�lB��B��B��B��B��B��B�lB�6B�6B̗BһBѶBЯBѶBҼB�#B�(B��B��B��B�6B�rB�xB�~B	B	;B	AB	AB	5B	"B	B	wB	 �B	�B	xB	xB	rB	rB	xB	 �B	#�B	(�B	.�B	1�B	3�B	6
B	7B	8B	;(B	;(B	;(B	=5B	@GB	BSB	AMB	BSB	8B	6B	7B	8B	:#B	=5B	>;B	?AB	?AB	?AB	@GB	BSB	CZB	CZB	EfB	HxB	I~B	K�B	M�B	O�B	P�B	Q�B	U�B	X�B	Y�B	Z�B	\�B	cB	g0B	h6B	jAB	lMB	nZB	o`B	qlB	sxB	t~B	u�B	w�B	y�B	z�B	z�B	{�B	}�B	�B	��B	��B	��B	��B	�B	�B	�)B	�)B	�6B	�AB	�MB	�`B	�rB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�*B	�0B	�6B	�6B	�<B	�<B	�6B	�<B	�6B	�BB	�ZB	�fB	�lB	�lB	�B	˅B	˅B	˅B	˅B	͑B	͑B	ΗB	ΗB	ϝB	УB	УB	ѪB	ѪB	УB	ѪB	ѪB	ѪG�O�B	�uB	�:B	�UB

bB
�B
 �B
)�B
-1B
3"B
8�B
@�B
CB
J,B
OeB
R�B
XgB
^qB
c�B
g�B
mB
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144182022020411441820220204114418  AO  ARCAADJP                                                                    20200619170912    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170912  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170912  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114418  IP                  G�O�G�O�G�O�                