CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:56Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041156  20190604094026  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��_���1   @��`ò��@4a�7Kƨ�d畁$�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�D��D�S3D�x D��RD�RD�5qD�t{D�޸D��3D�E�D���D��
D��)D�EqDږ�D�ƸD��fD�+�D�]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@�p�@�p�A�RA=�A^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�BpzBw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C\C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D#GD#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dm�GDnGDnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Dy��D�>D�P�D�uqD���D��D�2�D�q�D��)D���D�C4D��D��{D���D�B�Dڔ)D��)D���D�(�D�Z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�AǁA�z�A�z�A�z�AǁAǇ+AǋDAǍPA��AőhA���A�ƨAģ�AēuA�x�A�=qA�1'A�+A�%A��/Aú^A��A�=qA��A��A��+A�O�A�bA��TA���A���A�VA�JA�A��\A�hsA�/A��A��FA��A��A��+A���A��FA�dZA�1'A��yA���A�;dA�~�A��^A��-A���A���A�=qA�A�bNA�A�ĜA���A��A��A�O�A��HA�{A��A�oA�/A��A���A��RA�dZA��^A�l�A�;dA��A�9XA�ffA�bA��!A���A���A�bA��jA�"�A�XA��A��A��^A��FA��TA��^A�v�A�A���A�ƨA�Q�A��A��;A��PA�;dA��/A�%A�G�A�l�A�;dA��yA�O�A�oA��A�33A��A}�mA{G�AzbAyAx�\Ax-Aw��AwXAw/Aw�AvffAuK�AtffAs��Ar��Ap�HAo�An�Al�HAk�Ak��AkoAh��Af�Ac�A`��A^1'A\M�A[S�AZ  AW�AV1AUt�AT�\ARffAQ&�AP�AO�7AM�mAL��ALAK/AH�AGS�AE�-AC33AA+A>��A<�yA8�A6�\A5|�A4ZA3�-A3�hA3t�A2�`A0Q�A/�A/��A/XA.��A-�FA,��A,�A+�^A*�+A)��A)7LA&��A&ZA&{A%�TA%��A%\)A%�A#�A!��A  �AhsA��A�AI�A�#A�A?}A�A�A~�A-A��AhsA��A��A�jAAC�A�uA5?A�
Ax�A�uA`BA�A��A��Ap�A	��A	XAĜA�hAbNA33A�9A��Az�A\)A Z@���@�S�@��\@��u@�K�@���@���@��H@���@�1'@�C�@�n�@�?}@�l�@@웦@���@��T@�/@�ȴ@��T@ܓu@���@�E�@��@؃@� �@��;@�l�@�v�@�{@�{@��#@��@�$�@�@���@��#@���@�A�@�@��@�V@� �@�ƨ@�|�@�o@ʧ�@�=q@ɩ�@��@��@ƸR@�M�@���@ř�@�X@�7L@���@Ĵ9@���@��y@§�@�{@�@���@���@�Ĝ@���@�=q@�O�@�bN@� �@���@�{@�`B@���@�Q�@��m@��
@�ƨ@��P@�C�@��@��h@�A�@�l�@��H@��!@�~�@�$�@��7@���@��D@��@�t�@��y@�=q@�$�@��^@�z�@�1'@��@���@�@�J@��7@�Z@�ƨ@��@�ff@�=q@��@��#@���@�x�@�V@���@��;@�l�@�o@��@���@�v�@�M�@�=q@��@��#@��-@�?}@�j@��w@��@��!@�M�@�@�X@�&�@�&�@�/@��@�&�@���@��@��@���@��@�Ĝ@��D@�Z@�(�@���@���@��@���@�@���@��\@�v�@�ff@�5?@��#@��h@��@��/@��u@�j@�A�@��@��
@���@���@���@���@��
@�  @�1@��@��@���@���@��@�l�@�S�@��@��y@���@���@��+@�~�@�n�@�ff@�ff@�^5@�E�@�J@�@���@��-@��h@�X@�V@���@��@��`@���@��j@��@���@��u@��D@�1'@���@��@��
@�|�@���@�^5@���@�p�@�X@��@���@��`@���@��@��D@� �@��w@�l�@�o@��@�v�@��#@���@�hs@��@��9@�I�@�ƨ@�K�@�"�@���@��H@��H@�ȴ@�@���@�hs@�&�@�V@���@��`@��`@��/@�Ĝ@���@�j@�^�@���@}c@v�L@n5?@d%�@Y%@P_@G"�@C{J@?n/@9@1�M@*��@$"h@�D@��@9�@hs@2�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A�z�AǁA�z�A�z�A�z�AǁAǇ+AǋDAǍPA��AőhA���A�ƨAģ�AēuA�x�A�=qA�1'A�+A�%A��/Aú^A��A�=qA��A��A��+A�O�A�bA��TA���A���A�VA�JA�A��\A�hsA�/A��A��FA��A��A��+A���A��FA�dZA�1'A��yA���A�;dA�~�A��^A��-A���A���A�=qA�A�bNA�A�ĜA���A��A��A�O�A��HA�{A��A�oA�/A��A���A��RA�dZA��^A�l�A�;dA��A�9XA�ffA�bA��!A���A���A�bA��jA�"�A�XA��A��A��^A��FA��TA��^A�v�A�A���A�ƨA�Q�A��A��;A��PA�;dA��/A�%A�G�A�l�A�;dA��yA�O�A�oA��A�33A��A}�mA{G�AzbAyAx�\Ax-Aw��AwXAw/Aw�AvffAuK�AtffAs��Ar��Ap�HAo�An�Al�HAk�Ak��AkoAh��Af�Ac�A`��A^1'A\M�A[S�AZ  AW�AV1AUt�AT�\ARffAQ&�AP�AO�7AM�mAL��ALAK/AH�AGS�AE�-AC33AA+A>��A<�yA8�A6�\A5|�A4ZA3�-A3�hA3t�A2�`A0Q�A/�A/��A/XA.��A-�FA,��A,�A+�^A*�+A)��A)7LA&��A&ZA&{A%�TA%��A%\)A%�A#�A!��A  �AhsA��A�AI�A�#A�A?}A�A�A~�A-A��AhsA��A��A�jAAC�A�uA5?A�
Ax�A�uA`BA�A��A��Ap�A	��A	XAĜA�hAbNA33A�9A��Az�A\)A Z@���@�S�@��\@��u@�K�@���@���@��H@���@�1'@�C�@�n�@�?}@�l�@@웦@���@��T@�/@�ȴ@��T@ܓu@���@�E�@��@؃@� �@��;@�l�@�v�@�{@�{@��#@��@�$�@�@���@��#@���@�A�@�@��@�V@� �@�ƨ@�|�@�o@ʧ�@�=q@ɩ�@��@��@ƸR@�M�@���@ř�@�X@�7L@���@Ĵ9@���@��y@§�@�{@�@���@���@�Ĝ@���@�=q@�O�@�bN@� �@���@�{@�`B@���@�Q�@��m@��
@�ƨ@��P@�C�@��@��h@�A�@�l�@��H@��!@�~�@�$�@��7@���@��D@��@�t�@��y@�=q@�$�@��^@�z�@�1'@��@���@�@�J@��7@�Z@�ƨ@��@�ff@�=q@��@��#@���@�x�@�V@���@��;@�l�@�o@��@���@�v�@�M�@�=q@��@��#@��-@�?}@�j@��w@��@��!@�M�@�@�X@�&�@�&�@�/@��@�&�@���@��@��@���@��@�Ĝ@��D@�Z@�(�@���@���@��@���@�@���@��\@�v�@�ff@�5?@��#@��h@��@��/@��u@�j@�A�@��@��
@���@���@���@���@��
@�  @�1@��@��@���@���@��@�l�@�S�@��@��y@���@���@��+@�~�@�n�@�ff@�ff@�^5@�E�@�J@�@���@��-@��h@�X@�V@���@��@��`@���@��j@��@���@��u@��D@�1'@���@��@��
@�|�@���@�^5@���@�p�@�X@��@���@��`@���@��@��D@� �@��w@�l�@�o@��@�v�@��#@���@�hs@��@��9@�I�@�ƨ@�K�@�"�@���@��H@��H@�ȴ@�@���@�hs@�&�@�V@���@��`@��`@��/@�Ĝ@���G�O�@�^�@���@}c@v�L@n5?@d%�@Y%@P_@G"�@C{J@?n/@9@1�M@*��@$"h@�D@��@9�@hs@2�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBaHBaHBbNBbNBbNBbNBaHBaHB_;BXBK�BP�BW
BYB[#B]/B`BB_;B_;B`BBcTBdZBw�B�7B�JB�VB�{B��B��B��B��B��B��B�B�3B�FB�XB�^B�^B�dB�qB�}B�wB�qBĜBȴB��B��B��B�B��B��B�B�B�B�B�B�B�#B�B��B��B��B�RB��B��B�PB�B|�Bq�BgmB^5BQ�BL�BI�BG�BA�B;dB2-B#�B�B\BDB%BB��B�B�B�B�HB�LB��B�\B}�Bk�B\)BN�BH�BE�BA�B=qB8RB1'B"�BVB
��B
�NB
��B
ĜB
�3B
�B
��B
��B
�hB
~�B
u�B
m�B
iyB
ffB
cTB
bNB
bNB
bNB
^5B
W
B
P�B
K�B
D�B
8RB
.B
'�B
�B
�B
{B
\B
  B	�B	�/B	ȴB	�RB	�B	��B	��B	�\B	�%B	�B	{�B	q�B	k�B	hsB	bNB	ZB	R�B	M�B	G�B	<jB	49B	+B	�B	�B	DB	  B�B�B�sB�ZB�NB�NB�BB�)B��B��B��B��B��BȴBŢBÖB��B�qB�dB�LB�'B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B�uB�PB�7B�+B�%B�B�B�B�B� B}�B|�B{�Bz�By�Bx�Bw�Bu�Bt�Bt�Bs�Br�Bp�Bq�Bo�Bn�Bm�Bm�Bl�Bk�Bk�BjBk�Bk�Bk�BjBiyBk�Bl�Bm�Bp�Bp�Bs�Bu�Bu�Bv�By�B|�B}�B� B�B�VB�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�-B�3B�XB�jB�qB��BĜBǮBɺBɺB��B��B��B��B��B��B��B��B�B�NB�mB�B�B�B�B��B��B	  B	B	B	+B	+B	1B		7B		7B	
=B	\B	{B	�B	�B	�B	�B	�B	�B	!�B	$�B	'�B	+B	.B	5?B	8RB	9XB	=qB	?}B	?}B	@�B	C�B	J�B	M�B	R�B	S�B	VB	YB	YB	ZB	[#B	\)B	\)B	`BB	cTB	e`B	iyB	l�B	m�B	m�B	n�B	n�B	o�B	o�B	q�B	r�B	u�B	z�B	}�B	�B	�B	�%B	�7B	�JB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�3B	�3B	�3B	�9B	�9B	�LB	�RB	�dB	�jB	�qB	�}B	�}B	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�BB	�HB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
1B
1B
	7B
)B
_B
�B
(XB
0�B
6�B
B'B
G_B
NVB
RB
V�B
Z�B
`�B
f�B
l�B
r�B
u%B
v�B
|PB
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   BZBZB[$B[$B[$B[!BZBZBXBP�BD�BI�BO�BQ�BS�BVBYBXBXBYB\1B]3Bp�B�B�"B�/B�PB�hB��B��B��B��B��B��B�B�B�'B�2B�3B�9B�CB�PB�IB�AB�qB��BĝBɴB��B��B��B��B��B��B��B��B��B��B��B��B��BÖB�`B�.B��B�aB�'B}�Bu�Bj�B`JBWBJ�BE�BB�B@�B:iB4CB+B�BdB<BB�B��B��B�B�B�iB�+B�4B��B�DBv�BdqBUBG�BA�B>�B:zB6_B1>B*B�BJB
�B
�DB
��B
��B
�-B
��B
��B
�{B
�dB
w�B
n�B
f�B
bxB
__B
\PB
[LB
[IB
[KB
W1B
PB
I�B
D�B
=�B
1QB
'B
 �B
�B
�B
|B
_B	�B	�B	�4B	��B	�\B	�B	��B	��B	�iB	.B	{B	t�B	j�B	d�B	a�B	[\B	S.B	L B	F�B	@�B	5xB	-IB	$B	�B	�B	ZB�B�B�B�B�pB�gB�eB�\B�@B�B�B��B��B��B��B��B��B��B��B��B�jB�CB�9B�5B�1B�-B�(B�!B�B��B��B��B��B��B��B��B��B�oB�VB�KBFB~=B}9B|1Bz'By#BwBvBuBtBr�Bq�Bp�Bn�Bm�Bm�Bl�Bk�Bi�Bj�Bh�Bg�Bf�Bf�Be�Bd�Bd�Bc�Bd�Bd�Bd�Bc�Bb�Bd�Be�Bf�Bi�Bi�Bl�Bn�Bn�Bo�Bs BvBwBy"B~BB�xB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B� B�"B�6B�?B�:B�FB�IB�QB�TB�yB��B��B��B��B��B��B��B��B��B��B��B�B�	B�B�B�)B�qB��B�B�B��B��B��B�B�"B�)B�<B	 HB	 KB	MB	UB	WB	]B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!B	$B	'/B	.XB	1nB	2xB	6�B	8�B	8�B	9�B	<�B	C�B	F�B	LB	MB	OB	R4B	R3B	S<B	T<B	UFB	UEB	Y`B	\pB	^zB	b�B	e�B	f�B	f�B	g�B	g�B	h�B	h�B	j�B	k�B	n�B	s�B	wB	{'B	}6B	>B	�PB	�cB	�}B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�#B	�3B	�7B	�@B	�KB	�NB	�JB	�LB	�SB	�TB	�dB	�nB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�'B	�.B	�/B	�.B	�+B	�0B	�:B	�KB	�TB	�^B	�lB	�jB	�rB	�tB	�uB	�}B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�1B	�:B	�=B	�9B
 ?B
 >B
 >B
 AB
 AB
BB
EG�O�B
@B
rB
�B
!jB
)�B
0B
;9B
@qB
GiB
KB
O�B
S�B
Y�B
_�B
e�B
k�B
n8B
o�B
ubB
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.007(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940262019060409402620190604094026  AO  ARCAADJP                                                                    20181121041156    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041156  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041156  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094026  IP                  G�O�G�O�G�O�                