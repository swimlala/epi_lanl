CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-24T02:15:17Z AOML 3.0 creation; 2016-05-31T19:14:44Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150824021517  20160531121445  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               zA   AO  4051_7090_122                   2C  D   APEX                            5368                            041511                          846 @�i�,�6�1   @�i� @3z��vȴ�d`�t�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    zA   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dy� D�	�D�C3D�� D���D��D�C3D�9�D�� D� D�I�D�vfD�� D��D�I�DږfD�ɚD���D�33D�i�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
>@�=q@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)BzBzB�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B��
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
=B���B��
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
C�C�C�C�C	�C�C�C�C��C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�CpCq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��DtT{Dy��D�D�@�D��qD��>D�
>D�@�D�7D��qD�qD�GD�s�D��qD�
>D�GDړ�D��D��>D�0�D�gD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A敁A�ZA���A�bNA�-A�=qA���A��A�|�A�K�A��TA� �A��
A�Q�Aߛ�A��#A�p�A�A�VAډ7A�Q�A�x�A��A�bA���A�1A�ƨA�AΓuA���A�5?AȾwA�hsA���A���A�r�A��jA���A���A�bNA�$�A���A��-A�A��PA��A��A���A�A���A�/A�XA�1'A��A��A�1A�"�A���A��\A�dZA��9A�5?A���A���A��HA�A�VA��jA��hA�p�A�K�A�bA��
A��A�"�A�I�A�A�A��A��\A���A�oA��^A�%A�dZA���A�|�A�\)A�A���A��;A��A��9A�ZA�A�A�33A�n�A��!A�K�A��A�=qA�Q�A��uA��A}��Ax1'Au��AsO�Ap�+An�Al�9Aj�HAiS�Af�`A`�jA_�^A_l�A^E�A\jAY�AVĜAS�wAO�mAO&�AN~�AM�AL�AJ1'AH�+AG�AES�AC��AA�AAXA@��A>��A=XA:=qA9oA7��A7ƨA7C�A5��A3��A21A0Q�A/�^A/x�A/A.z�A-�A,��A+"�A*�uA*�A)G�A'�A'�wA'�A&$�A%�-A$�A$bA#�A#%A"��A �`A��AC�A�mAO�A�TAl�AS�A
=A�A�9A�;A��AQ�A��A�A�+A�;A�AO�A/An�AA�A�hA�7A\)At�A
z�A	��A	dZAS�A�A��AM�AVA	"�A	�TA	dZAp�A�uA�mAA�PA33AȴA�!An�A;dA�A M�@��@�C�@���@��T@��D@���@��;@�O�@��@�Z@�j@�u@�C�@�33@��@�1'@�^5@�j@݁@�I�@ڏ\@���@�t�@���@�Z@��y@�j@мj@�1'@��
@ϥ�@�(�@�K�@��@̛�@�Ĝ@��@ˮ@�C�@�33@�+@�;d@�C�@ʏ\@ȼj@Ǖ�@�ȴ@ź^@�V@�Q�@Ý�@��@�$�@�`B@��`@�b@���@�S�@���@��@��T@���@��^@�x�@�O�@��`@�\)@��\@���@�G�@���@�b@�t�@�
=@�~�@��@�/@���@���@��@���@�V@��u@�Q�@���@�S�@�@���@�n�@�@�?}@��u@�bN@��m@�l�@�K�@�33@���@���@��@�G�@�&�@�%@��j@��@�(�@��;@���@�l�@�\)@�"�@��H@��\@�M�@�$�@�=q@�{@��T@�`B@�X@���@�Ĝ@�(�@��F@��P@�S�@�o@�@��@��!@�v�@��@���@�7L@��/@�z�@�A�@�1@�1@��@��m@��w@��@���@�l�@�@��y@�ȴ@��+@�E�@��@���@���@��-@�O�@���@��j@��@�9X@���@�K�@�"�@�o@��y@��!@��+@�ff@�-@�{@�J@��T@��^@���@��@�/@���@��@�1'@���@�o@���@���@���@�n�@�M�@�{@�@�X@��@��@��@���@���@��P@�|�@�S�@�;d@�+@���@��y@���@�ȴ@���@���@��h@��7@�X@���@���@�t�@�C�@�@���@���@��\@�^5@�J@��@���@�x�@�X@�G�@�?}@�O�@�?}@�7L@�O�@�V@��@��9@�  @��w@��m@��@��@�;d@���@�M�@�E�@�J@���@�x�@�G�@���@���@�j@�1'@�b@�  @���@���@��m@���@��@��P@���@���@��+@�M�@��@��-@���@�(�@+@sdZ@k�F@d(�@X�9@PQ�@I��@>�+@8r�@1hs@,�@%p�@ bN@�@@��@|�@t�@r�@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���A敁A�ZA���A�bNA�-A�=qA���A��A�|�A�K�A��TA� �A��
A�Q�Aߛ�A��#A�p�A�A�VAډ7A�Q�A�x�A��A�bA���A�1A�ƨA�AΓuA���A�5?AȾwA�hsA���A���A�r�A��jA���A���A�bNA�$�A���A��-A�A��PA��A��A���A�A���A�/A�XA�1'A��A��A�1A�"�A���A��\A�dZA��9A�5?A���A���A��HA�A�VA��jA��hA�p�A�K�A�bA��
A��A�"�A�I�A�A�A��A��\A���A�oA��^A�%A�dZA���A�|�A�\)A�A���A��;A��A��9A�ZA�A�A�33A�n�A��!A�K�A��A�=qA�Q�A��uA��A}��Ax1'Au��AsO�Ap�+An�Al�9Aj�HAiS�Af�`A`�jA_�^A_l�A^E�A\jAY�AVĜAS�wAO�mAO&�AN~�AM�AL�AJ1'AH�+AG�AES�AC��AA�AAXA@��A>��A=XA:=qA9oA7��A7ƨA7C�A5��A3��A21A0Q�A/�^A/x�A/A.z�A-�A,��A+"�A*�uA*�A)G�A'�A'�wA'�A&$�A%�-A$�A$bA#�A#%A"��A �`A��AC�A�mAO�A�TAl�AS�A
=A�A�9A�;A��AQ�A��A�A�+A�;A�AO�A/An�AA�A�hA�7A\)At�A
z�A	��A	dZAS�A�A��AM�AVA	"�A	�TA	dZAp�A�uA�mAA�PA33AȴA�!An�A;dA�A M�@��@�C�@���@��T@��D@���@��;@�O�@��@�Z@�j@�u@�C�@�33@��@�1'@�^5@�j@݁@�I�@ڏ\@���@�t�@���@�Z@��y@�j@мj@�1'@��
@ϥ�@�(�@�K�@��@̛�@�Ĝ@��@ˮ@�C�@�33@�+@�;d@�C�@ʏ\@ȼj@Ǖ�@�ȴ@ź^@�V@�Q�@Ý�@��@�$�@�`B@��`@�b@���@�S�@���@��@��T@���@��^@�x�@�O�@��`@�\)@��\@���@�G�@���@�b@�t�@�
=@�~�@��@�/@���@���@��@���@�V@��u@�Q�@���@�S�@�@���@�n�@�@�?}@��u@�bN@��m@�l�@�K�@�33@���@���@��@�G�@�&�@�%@��j@��@�(�@��;@���@�l�@�\)@�"�@��H@��\@�M�@�$�@�=q@�{@��T@�`B@�X@���@�Ĝ@�(�@��F@��P@�S�@�o@�@��@��!@�v�@��@���@�7L@��/@�z�@�A�@�1@�1@��@��m@��w@��@���@�l�@�@��y@�ȴ@��+@�E�@��@���@���@��-@�O�@���@��j@��@�9X@���@�K�@�"�@�o@��y@��!@��+@�ff@�-@�{@�J@��T@��^@���@��@�/@���@��@�1'@���@�o@���@���@���@�n�@�M�@�{@�@�X@��@��@��@���@���@��P@�|�@�S�@�;d@�+@���@��y@���@�ȴ@���@���@��h@��7@�X@���@���@�t�@�C�@�@���@���@��\@�^5@�J@��@���@�x�@�X@�G�@�?}@�O�@�?}@�7L@�O�@�V@��@��9@�  @��w@��m@��@��@�;d@���@�M�@�E�@�J@���@�x�@�G�@���@���@�j@�1'@�b@�  @���@���@��m@���@��@��P@���@���@��+@�M�@��@��-@���@�(�@+@sdZ@k�F@d(�@X�9@PQ�@I��@>�+@8r�@1hs@,�@%p�@ bN@�@@��@|�@t�@r�@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
}�B
}�B
|�B
}�B
~�B
� B
�B
�B
�%B
�1B
�hB
��B
��B
ĜB
��B�B0!B/BA�BQ�B\)Bw�B�%B�B&�B|�B�7B��B�RB�}B��B��B�mB�B��BVB1B��B��B�TB�`B�ZB��B�HB�HB�HB�B��B��B��B
=B"�B2-B6FB7LBuBB�B��B�TB�B�B5?B49B2-B(�B�BuB)�B33B,B-B)�B;dB@�B;dB2-B2-B33B0!B-B)�B$�B{BB�fB��B�RB��B�=Bq�B`BB`BB>wBVB��B�NBĜB�-B�{Br�BVB=qB(�B�B+B
��B
�BB
ÖB
��B
�hB
�B
gmB
N�B
%�B
\B	��B	�BB	��B	�qB	�!B	��B	��B	}�B	v�B	s�B	k�B	_;B	N�B	<jB	+B	�B	�B	�B	bB	
=B��B��B�B�`B�BB�5B�/B�B��B��B��B��B��B��BȴBÖB��B�qB�jB�dB�dB�XB�RB�LB�9B�!B�B�B��B��B��B��B��B��B�uB�hB�bB�VB�DB�+B�B�B}�B� B�=B�JB�JB�VB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�FB�RB�-B�B�B�B��B��B��B��B��B�-B��B�;B�#B��B��B��B��BɺBȴBƨB��B��B��BŢB��B�wB�qB�jB�^B�FB�9B�!B�B��B��B��B�\B�\B�VB�PB�JB�DB�=B�JB�JB�PB�VB�hB�{B��B��B��B��B��B��B�B�'B�LB�RB�dBB��B��B��BBBBĜBĜBŢB��B��B��B��B��B��B��B�B�
B�B�B�/B�/B�;B�HB�TB�TB�ZB�fB�sB�B�B�B�B�B��B��B��B��B	B	B	1B	DB	\B	uB	�B	�B	�B	!�B	$�B	'�B	(�B	+B	,B	-B	1'B	6FB	8RB	;dB	?}B	@�B	A�B	C�B	F�B	K�B	R�B	R�B	R�B	S�B	T�B	VB	YB	[#B	]/B	]/B	^5B	^5B	`BB	bNB	cTB	e`B	ffB	jB	p�B	p�B	s�B	s�B	v�B	w�B	x�B	z�B	� B	�B	�B	�%B	�+B	�7B	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�3B	�9B	�9B	�?B	�?B	�FB	�FB	�LB	�RB	�XB	�XB	�dB	�qB	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�HB	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
1B
1B
	7B
bB
�B
"�B
)�B
2-B
9XB
@�B
E�B
N�B
S�B
ZB
_;B
e`B
iyB
m�B
r�B
u�B
x�B
|�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
}�B
}�B
|�B
~ B
B
�B
�B
�#B
�0B
�9B
�tB
��B
��B
ĥB
�B�B0)B/#BA�BQ�B\/Bw�B�*B�"B&�B|�B�;B��B�ZB��B��B�B�yB��B�B`B9B��B��B�[B�fB�aB��B�OB�TB�SB�B�B��B��B
EB"�B29B6TB7[B}B!B�B��B�`B�B�B5IB4EB2:B(�B�B}B*B3?B,B-B*B;nB@�B;oB29B27B3=B0,B-B*B$�B�B&B�pB��B�]B��B�FBq�B`JB`IB>�B^B��B�TBģB�5B��Br�BVB=|B(�B�B4B
��B
�KB
àB
��B
�tB
�B
g{B
N�B
%�B
lB	��B	�UB	��B	��B	�3B	��B	��B	~	B	v�B	s�B	k�B	_PB	N�B	<�B	+B	�B	�B	�B	zB	
VB�B��B�B�zB�_B�RB�LB�0B�B�B�B�B��B��B��BòB��B��B��B��B��B�tB�nB�kB�UB�=B�3B�'B�B��B��B��B��B��B��B��B��B�tB�cB�KB�9B�.B~B� B�ZB�jB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B�&B�4B�DB�bB�kB�IB�4B�(B�+B�B��B��B��B�B�IB�B�YB�?B��B��B��B��B��B��B��B��B��B��BſB��B��B��B��B�zB�eB�SB�>B�$B�B��B��B�zB�{B�uB�oB�iB�dB�^B�hB�iB�pB�tB��B��B��B��B��B�B�B�B�%B�CB�iB�mB�B«B��B��B��B¬B©B©BķBĻBſB��B��B�B�B�B�B�B�B�&B�,B�6B�JB�IB�XB�cB�pB�nB�uB�B�B�B�B��B�B��B��B��B�B�B	"B	3B	JB	YB	vB	�B	�B	�B	�B	!�B	$�B	(B	)B	+B	,B	-$B	1?B	6\B	8jB	;}B	?�B	@�B	A�B	C�B	F�B	K�B	SB	S	B	SB	TB	UB	VB	Y.B	[;B	]EB	]DB	^LB	^MB	`WB	bgB	ciB	euB	f}B	j�B	p�B	p�B	s�B	s�B	v�B	w�B	x�B	z�B	�B	�(B	�4B	�:B	�>B	�KB	�iB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�!B	�%B	�-B	�;B	�GB	�FB	�KB	�MB	�QB	�SB	�YB	�XB	�_B	�dB	�jB	�kB	�vB	��B	��B	£B	ıB	ŷB	ƹB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�"B	�!B	�1B	�:B	�AB	�@B	�JB	�HB	�LB	�MB	�SB	�WB	�ZB	�ZB	�UB	�[B	�_B	�fB	�oB	�jB	�kB	�jB	�kB	�qB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	�B
 B
B
B
B
B
B
B
B
 B
B
B
B
B
B
&B
(B
+B
)B
)B
+B
-B
/B
7B
6B
5B
5B
6B
CB
AB
	DB
sB
�B
"�B
*B
2>B
9hB
@�B
E�B
N�B
TB
Z*B
_HB
eoB
i�B
m�B
r�B
u�B
x�B
|�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214452016053112144520160531121445  AO  ARCAADJP                                                                    20150824021517    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150824021517  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150824021517  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121445  IP                  G�O�G�O�G�O�                