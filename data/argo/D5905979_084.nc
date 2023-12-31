CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:14Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170914  20220204114419  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               TA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��L��31   @���G�@6~5?|��c�I�^51   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    TA   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A���A�33A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/y�D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DC��DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D�D�_
D��\D��3D�!�D�[�D��qD�߮D��D�]�D���Dǀ�D�#�D�VDږD��fD��D�Y�D�fD�Ϯ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�(�A�(�Aޏ\A�\)A�\)B�B�B�B�B'�B/�B7�B?G�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
B���B��
B��
B��
B��
B��
B��
B��
Bߣ�B��B��
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C C!�C#��C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/t{D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<t{D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DCGDCz�DC�{DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQt{DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DYGDYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt�{Dy��D��D�\{D���D�ФD�D�YHD���D��D�)D�[4D��gD�~D�!HD�S�Dړ�D���D�4D�WD��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��HA��yA��yA��A��A��yA��A��A��A��A��`Aə�A���A���AƑhA�$�Aß�A��9A���A��#A��PA��FA�{A�
=A�+A�A�dZA���A��#A�M�A��`A�
=A��A�|�A���A�ȴA���A��A�O�A�z�A���A��A���A�K�A�dZA�l�A�E�A�;dA���A�x�A� �A��A���A�jA�O�A���A���A��`A���A��A���A�S�A���A�jA�-A�%A�S�A�{A��-A��FA��9A��A��A���A��HA���A���A��RA���A�~�A�?}A��PA�hsA���A���A��+A�M�A��;A��/A�%A��A���A�{A�?}A���A��9A���A�jA���A��hA��A��A|�+Ay�Aw�7Au�
At�Aq��Ao�Am`BAk�wAj��Ai
=Ae�mAc�FAa��A`I�A^1'A\��A[�AZ-AYXAW��AV �AT��AS33AQ�AP~�AO�#AN�ALI�AJ��AIp�AGt�AD��AC�ABA@�9A?��A>��A=��A<��A;�hA:E�A8��A8Q�A7p�A4�A3t�A1��A0�HA0�DA/�7A/VA.9XA-;dA,��A,E�A,{A+C�A*A�A)33A(9XA'|�A$�+A#oA"-A ��A JA��AoA�\A1'A+AZA1A��AoA��A�DAv�AE�Ax�A�A�HAz�A1'A�
A�/A��A�A=qAdZA%AĜAƨA�;A	��A	&�AȴA9XAjA�Av�AjAI�A+A�;AK�A �RA bN@���@�x�@��m@���@�$�@��-@�hs@�V@��F@���@��`@�9X@�@�@�P@�dZ@��@�-@㕁@��@�v�@��T@�%@�j@��@���@�@�v�@ܓu@�1@��@�Q�@Ӿw@�;d@ҸR@�v�@�-@�@�G�@��/@��@�E�@�5?@���@�z�@�Q�@˝�@�\)@���@��T@�X@�l�@���@�^5@�hs@��`@��9@�j@�|�@�V@�Q�@��@��F@�|�@�C�@�@��\@�$�@�`B@���@��9@�r�@��
@��P@���@���@�\)@��\@�=q@�E�@�M�@�=q@�$�@�J@��@���@���@��@���@��u@�9X@��H@�-@�X@��@�+@���@��@��-@��@�=q@�^5@��+@�^5@��+@�G�@�I�@�@�ȴ@���@��@���@��h@�x�@�%@��@�1'@��;@�|�@��y@��+@�J@��h@��D@�Z@�I�@�A�@�1'@�(�@� �@�1@��@���@��@�l�@��@�
=@�o@�@��!@��!@�v�@�E�@�5?@�V@�V@���@�X@�O�@���@���@��-@��#@��@���@�G�@���@�Z@� �@��@��@�  @���@�z�@��j@��D@��@�9X@��@��@��m@��m@�S�@�ȴ@�v�@�E�@��@��-@�%@� �@�=q@�=q@��\@���@�@��y@�v�@��@��^@�p�@��j@�t�@��@�~�@�-@��@��#@��h@�hs@�`B@�O�@��/@��D@�bN@�9X@� �@�(�@�(�@�I�@�z�@�Ĝ@���@�p�@�@��@���@��@���@���@�Ĝ@�dZ@���@�{@���@�@�?}@��@�z�@��;@��@���@���@���@�|�@�K�@�+@�
=@���@��!@��\@�M�@��-@�&�@���@��9@���@���@�z�@�I�@��m@�dZ@�o@�n�@�@���@��h@�O�@�&�@��@�Ĝ@�I�@�  @���@�t�@���@�M�@�5?@�{@��-@�hs@�G�@��`@�I�@�b@�@\)@+@y��@u�@r�@j�+@a��@]^�@W� @P�@I@?�q@:�@6�"@0�.@);@#s@�@�$@PH@I�@B�@l"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��TA��HA��yA��yA��A��A��yA��A��A��A��A��`Aə�A���A���AƑhA�$�Aß�A��9A���A��#A��PA��FA�{A�
=A�+A�A�dZA���A��#A�M�A��`A�
=A��A�|�A���A�ȴA���A��A�O�A�z�A���A��A���A�K�A�dZA�l�A�E�A�;dA���A�x�A� �A��A���A�jA�O�A���A���A��`A���A��A���A�S�A���A�jA�-A�%A�S�A�{A��-A��FA��9A��A��A���A��HA���A���A��RA���A�~�A�?}A��PA�hsA���A���A��+A�M�A��;A��/A�%A��A���A�{A�?}A���A��9A���A�jA���A��hA��A��A|�+Ay�Aw�7Au�
At�Aq��Ao�Am`BAk�wAj��Ai
=Ae�mAc�FAa��A`I�A^1'A\��A[�AZ-AYXAW��AV �AT��AS33AQ�AP~�AO�#AN�ALI�AJ��AIp�AGt�AD��AC�ABA@�9A?��A>��A=��A<��A;�hA:E�A8��A8Q�A7p�A4�A3t�A1��A0�HA0�DA/�7A/VA.9XA-;dA,��A,E�A,{A+C�A*A�A)33A(9XA'|�A$�+A#oA"-A ��A JA��AoA�\A1'A+AZA1A��AoA��A�DAv�AE�Ax�A�A�HAz�A1'A�
A�/A��A�A=qAdZA%AĜAƨA�;A	��A	&�AȴA9XAjA�Av�AjAI�A+A�;AK�A �RA bN@���@�x�@��m@���@�$�@��-@�hs@�V@��F@���@��`@�9X@�@�@�P@�dZ@��@�-@㕁@��@�v�@��T@�%@�j@��@���@�@�v�@ܓu@�1@��@�Q�@Ӿw@�;d@ҸR@�v�@�-@�@�G�@��/@��@�E�@�5?@���@�z�@�Q�@˝�@�\)@���@��T@�X@�l�@���@�^5@�hs@��`@��9@�j@�|�@�V@�Q�@��@��F@�|�@�C�@�@��\@�$�@�`B@���@��9@�r�@��
@��P@���@���@�\)@��\@�=q@�E�@�M�@�=q@�$�@�J@��@���@���@��@���@��u@�9X@��H@�-@�X@��@�+@���@��@��-@��@�=q@�^5@��+@�^5@��+@�G�@�I�@�@�ȴ@���@��@���@��h@�x�@�%@��@�1'@��;@�|�@��y@��+@�J@��h@��D@�Z@�I�@�A�@�1'@�(�@� �@�1@��@���@��@�l�@��@�
=@�o@�@��!@��!@�v�@�E�@�5?@�V@�V@���@�X@�O�@���@���@��-@��#@��@���@�G�@���@�Z@� �@��@��@�  @���@�z�@��j@��D@��@�9X@��@��@��m@��m@�S�@�ȴ@�v�@�E�@��@��-@�%@� �@�=q@�=q@��\@���@�@��y@�v�@��@��^@�p�@��j@�t�@��@�~�@�-@��@��#@��h@�hs@�`B@�O�@��/@��D@�bN@�9X@� �@�(�@�(�@�I�@�z�@�Ĝ@���@�p�@�@��@���@��@���@���@�Ĝ@�dZ@���@�{@���@�@�?}@��@�z�@��;@��@���@���@���@�|�@�K�@�+@�
=@���@��!@��\@�M�@��-@�&�@���@��9@���@���@�z�@�I�@��m@�dZ@�o@�n�@�@���@��h@�O�@�&�@��@�Ĝ@�I�@�  @���@�t�@���@�M�@�5?@�{@��-@�hs@�G�@��`@�I�@�b@�@\)G�O�@y��@u�@r�@j�+@a��@]^�@W� @P�@I@?�q@:�@6�"@0�.@);@#s@�@�$@PH@I�@B�@l"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB-B-B-B-B-B-B.B-B.B33B�DB�B/B=qBK�BI�B_;Bv�B��B��B��B��B�B�jBɺB�B�`B�sB�B��B��B  B�B&�B1'Bt�B�+Bk�B~�B�uB�B��B��B�5B��BɺBĜB�LB�{B}�B�1B�JB�DB�=B�uB��B��B�{B�7B�1B|�B_;B6FB-B'�B#�B�BVB\B
=B��B�BB��BȴB�'B��B�uB�Bn�BjBgmB[#BL�B@�B'�BhB
��B
��B
�B
�TB
�B
�B
�5B
��B
�fB
�;B
��B
��B
�B
s�B
`BB
T�B
B�B
(�B
{B
B	��B	�B	�/B	��B	ÖB	�wB	�XB	�B	��B	�B	t�B	k�B	`BB	T�B	K�B	B�B	>wB	5?B	'�B	 �B	�B	VB	+B	B��B�B�sB�;B�BȴBĜB�qB�XB�9B�'B�B�B��B��B��B��B��B��B��B�JB�B�DB�+B�B�+B� B�B� B�B|�B}�B|�B|�B~�Bx�Bl�BiyBiyBhsBhsBe`BdZBbNBe`Be`Be`BgmBgmBgmBgmBgmBhsBjBgmBbNBbNBaHB`BB\)BYBS�BR�BO�BL�BJ�BH�BE�BE�BC�BC�BC�BD�BC�B@�BA�B@�BB�B?}B>wB<jB;dB<jB<jB;dB;dB;dB;dB<jB>wBC�BF�BF�BF�BI�BI�BG�BI�BQ�BR�BZBcTBn�Bq�Bt�Bu�Bw�Bx�Bu�Bs�Bn�BjBffBe`BffBhsBl�Bo�Br�Bu�Bv�Bv�Bx�B{�B}�B� B�B�+B�+B�%B�1B�1B�+B�%B}�Bw�Bv�Bu�Bv�Bv�By�B�B�DB�PB�VB�VB�VB�\B�hB�hB�oB�hB�bB�\B�\B�\B�\B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�3B�FB�qB�wBÖBȴB��B�B�;B�BB�HB�`B�yB�TB�`B�ZB�mB�sB�B�B�B�B�B�B��B��B��B	  B	B	B	1B	PB	\B	\B	bB	bB	hB	hB	oB	{B	�B	�B	�B	�B	�B	�B	"�B	%�B	+B	33B	8RB	;dB	<jB	=qB	>wB	@�B	C�B	K�B	O�B	Q�B	S�B	T�B	T�B	W
B	YB	ZB	_;B	bNB	gmB	jB	n�B	v�B	{�B	}�B	~�B	~�B	�B	�%B	�1B	�=B	�VB	�\B	�\B	�bB	�bB	�\B	�PB	�JB	�JB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�FB	�RB	�^B	��B	ǮB	ȴB	ƨB	ŢB	ƨB	ĜB	��B	��B	�wB	�}B	��B	�}B	�wB	�qB	�qB	�wB	�wB	�wB	�wB	�wB	�}B	�}B	��B	��B	��B	B	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�ZB	�ZB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B
B
�B
�B
�B
)_B
0�B
6zB
:xB
C�B
HB
K�B
OvB
UMB
[WB
a�B
fLB
k�B
p�B
r�B
x�111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B%�B%�B%�B%�B%�B%�B&�B%�B&�B+�G�O�G�O�B'�B5�BD=BB0BW�Bo=B��B�$B�7B�nB��B��B�+B�tB��B��B�B�2B�2B�nB�BUB)�Bm$B�Bc�BwbB��B��B��B�HB֗B�OB�B� B��B��Bv]B��B��B��B��B��B�B�B��B��B��BuXBW�B.�B%}B _BFBB�B�B�B�MBضB�mB�*B��B�6B��By�BgBb�B_�BS�BEKB9B pB	�B
�dB
�MB
�"B
��B
ңB
ΊB
ֻB
�SB
��B
��B
�TB
�B
y�B
lCB
X�B
M�B
;B
!�B
B	��B	�jB	�!B	��B	�qB	�/B	�B	��B	��B	�)B	{�B	mZB	d$B	X�B	M�B	DhB	;1B	7B	-�B	 �B	iB	&B	�B��B��B��B�RB�B��BкB�_B�GB�B�B��B��B��B��B��B�yB�`B�TB�B�mB�<B��B|�B��B�B}�B�Bx�B{�Bx�Bz�Bu�Bv�Bu�Bu�Bw�Bq�Be>Bb-Bb-Ba'Ba'B^B]B[B^B^B^B`!B`!B`!B`!B`!Ba'Bc3B`"B[B[BY�BX�BT�BQ�BL�BK�BH�BE�BCxBAlB>ZB>ZB<NB<NB<NB=UB<OB9<B:BB9<B;HB86B70B5$B4B5$B5$B4B4B4B4B5$B71B<PB?bB?bB?bBBtBBtB@hBBtBJ�BK�BR�B\BgQBjbBmtBn{Bp�Bq�Bn{BloBgQBc8B_ B^B_ Ba-BeEBhWBkiBn|Bo�Bo�Bq�Bt�Bv�Bx�Bz�B�B�B~�B��B��B�B~�Bv�Bp�Bo�Bn}Bo�Bo�Br�B}�B��B�	B�B�B�B�B�!B�!B�(B�!B�B�B�B�B�B�4B�:B�_B�wB�wB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�(B�.B�MB�kBǐB��B��B��B��B�B�.B�	B�B�B�"B�(B�@B�MB�YB�_B�eB�kB�~B��B��B��B��B��B	 �B	B	B	B		B		B	
B	
B	"B	.B	4B	:B	FB	kB	qB	qB	�B	�B	#�B	+�B	1B	4B	5B	6"B	7(B	94B	<GB	DwB	H�B	J�B	L�B	M�B	M�B	O�B	Q�B	R�B	W�B	Z�B	`B	c.B	gGB	owB	t�B	v�B	w�B	w�B	y�B	~�B	��B	��B	�B	�	B	�	B	�B	�B	�	B	��B	��B	��B	�B	�B	�.B	�:B	�?B	�LB	�LB	�RB	�^B	�^B	�SB	�GB	�GB	�MB	�^B	�dB	�wB	�wB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�.B	�XB	�^B	�RB	�LB	�RB	�FB	�.B	�.B	�"B	�(B	�4B	�(B	�"B	�B	�B	�"B	�"B	�"B	�"B	�"B	�(B	�(B	�.B	�4B	�4B	�:B	�GB	�YB	�eB	�lB	�qB	�qB	�qB	�wB	�}B	ǃB	ʖB	ͨB	кB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�"B	�(B	�(B	�.B	�4B	�:B	�:B	�:G�O�B	�1B	��B
<B
oB
YB
"B
)1B
/ B
3B
<�B
@�B
DRB
HB
M�B
S�B
Z�B
^�B
d*B
iIB
k�B
q`111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144192022020411441920220204114419  AO  ARCAADJP                                                                    20200619170914    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170914  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170914  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114419  IP                  G�O�G�O�G�O�                