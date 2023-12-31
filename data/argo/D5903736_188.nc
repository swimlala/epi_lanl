CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-06-30T07:01:02Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170630070102  20190604094028  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�\��!1   @��b��@4}�E���d�7KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�
�D�UD���D��
D��D�K3D��3D���D�D�@�D�~fD�߮D�{D�J�D�l)D���D�fD�I�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=q@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�Bz�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CVCW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt�Dy��D�RD�R�D�� D��{D� D�H�D���D��RD��D�>gD�{�D��D��D�H D�i�D��4D��D�G]D�D��g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aܙ�Aܝ�Aܝ�Aܗ�Aܕ�AܓuA܃A�hsA�n�A�A�A�&�A�{A�
=A���A��A��#A��A�A�ZA�ĜAң�A�&�A�v�A���A��A���A�?}A�~�A��TA��/A�E�A�^5A��A���A�ƨA�ffA�p�A�O�A���A��yA���A�t�A��-A�|�A���A���A��yA�\)A�G�A��;A�l�A��A�hsA�Q�A�A���A��wA���A���A���A�|�A�1A�bA�oA�"�A��A���A��A��mA���A�A�9XA�VA��A��RA���A�G�A��FA�n�A���A�A�bNA��7A�+A��A��yA��RA���A�I�A�C�A��uA�C�A��`A��7A�=qA��A�~�A�A�A�jA�A�v�A�=qA�ffA���A�XA�+A��A�VA��A}�-Az�yAv��At��Ap�Am�Al5?Aj�AiƨAgp�Aet�Ab1'A`��A_"�A^5?A]��A[��AYAWl�AU�AU`BAT~�AS��AO�mAN-AKAJ�HAI�AH�+AG�AE�AD��AD{AC�A@~�A=�-A<�!A;dZA9�A9|�A97LA7�A5ƨA4��A3�TA2ȴA29XA/%A,��A+��A+O�A*�A*  A&�A%��A%?}A$��A$I�A#�-A#C�A"ȴA"bNA!C�A��AbAG�A �AbNAdZA�RA�PA�#Ap�An�A��A�AG�A�A��A�\A��A��AK�A��A^5A{A�hA
�A	��A�+Ap�A33A�HA�#A%A��A�^A�+AXA 1'@���@�t�@�ff@��@��`@��@�^5@��@�?}@�@�G�@�1@�^5@�O�@�j@�w@�!@�{@���@�1@�K�@��T@�9X@��@�V@ᙚ@��/@ް!@�J@��/@��@�n�@�-@�J@ّh@��/@��m@Չ7@�K�@��@���@Η�@˕�@��@��@�ff@Ų-@�x�@�A�@��@�bN@�J@�1'@��
@�+@��H@��!@���@�v�@��@���@��y@�-@��@���@�p�@�G�@�7L@��@�bN@�b@���@��w@�dZ@���@���@�~�@�n�@�ff@�V@�$�@���@��`@�  @��F@�33@�33@�33@�"�@�
=@�
=@���@��@���@��\@�v�@��@�r�@� �@�1@���@��F@���@�\)@�;d@���@��R@���@�M�@�J@���@���@�G�@��@�A�@�(�@��@���@�ƨ@�|�@�K�@�+@��@�"�@�+@�;d@�+@�
=@��y@��+@�=q@��#@���@���@��h@��7@��-@���@��^@��h@�p�@���@�Q�@��F@�|�@�33@�@�33@�C�@���@�^5@�?}@�Ĝ@��m@�"�@��!@�~�@�M�@�{@��^@���@��7@���@�{@�=q@�^5@�^5@�5?@�J@��#@�@���@��h@�p�@�7L@���@��@�r�@�Z@�9X@��;@��@�dZ@�"�@�
=@���@�ff@�-@��@���@���@���@��@�p�@�7L@�&�@�&�@�/@�7L@�G�@�p�@��@�/@�V@�V@�V@��@��@�V@��/@�Z@�  @��F@���@��@�dZ@�@���@�ff@�{@��#@���@�hs@�?}@��@��@��9@�j@�I�@�(�@�  @��@��m@�ƨ@��@�|�@�\)@�K�@�
=@���@��!@��\@�E�@��@��@�{@�{@���@��^@��h@�G�@��9@�9X@��m@��;@���@�l�@�+@�o@��R@���@�^5@�E�@�=q@�$�@�$�@�J@��7@�O�@�&�@��`@���@�Ĝ@��D@�j@���@��@�|�@�dZ@�\)@��0@�e�@z�@s'�@i�@a�C@W�@O�@Io @C�W@<A�@6�H@.�@)��@!@@<6@s�@��@M@&�@�I111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aܙ�Aܝ�Aܝ�Aܗ�Aܕ�AܓuA܃A�hsA�n�A�A�A�&�A�{A�
=A���A��A��#A��A�A�ZA�ĜAң�A�&�A�v�A���A��A���A�?}A�~�A��TA��/A�E�A�^5A��A���A�ƨA�ffA�p�A�O�A���A��yA���A�t�A��-A�|�A���A���A��yA�\)A�G�A��;A�l�A��A�hsA�Q�A�A���A��wA���A���A���A�|�A�1A�bA�oA�"�A��A���A��A��mA���A�A�9XA�VA��A��RA���A�G�A��FA�n�A���A�A�bNA��7A�+A��A��yA��RA���A�I�A�C�A��uA�C�A��`A��7A�=qA��A�~�A�A�A�jA�A�v�A�=qA�ffA���A�XA�+A��A�VA��A}�-Az�yAv��At��Ap�Am�Al5?Aj�AiƨAgp�Aet�Ab1'A`��A_"�A^5?A]��A[��AYAWl�AU�AU`BAT~�AS��AO�mAN-AKAJ�HAI�AH�+AG�AE�AD��AD{AC�A@~�A=�-A<�!A;dZA9�A9|�A97LA7�A5ƨA4��A3�TA2ȴA29XA/%A,��A+��A+O�A*�A*  A&�A%��A%?}A$��A$I�A#�-A#C�A"ȴA"bNA!C�A��AbAG�A �AbNAdZA�RA�PA�#Ap�An�A��A�AG�A�A��A�\A��A��AK�A��A^5A{A�hA
�A	��A�+Ap�A33A�HA�#A%A��A�^A�+AXA 1'@���@�t�@�ff@��@��`@��@�^5@��@�?}@�@�G�@�1@�^5@�O�@�j@�w@�!@�{@���@�1@�K�@��T@�9X@��@�V@ᙚ@��/@ް!@�J@��/@��@�n�@�-@�J@ّh@��/@��m@Չ7@�K�@��@���@Η�@˕�@��@��@�ff@Ų-@�x�@�A�@��@�bN@�J@�1'@��
@�+@��H@��!@���@�v�@��@���@��y@�-@��@���@�p�@�G�@�7L@��@�bN@�b@���@��w@�dZ@���@���@�~�@�n�@�ff@�V@�$�@���@��`@�  @��F@�33@�33@�33@�"�@�
=@�
=@���@��@���@��\@�v�@��@�r�@� �@�1@���@��F@���@�\)@�;d@���@��R@���@�M�@�J@���@���@�G�@��@�A�@�(�@��@���@�ƨ@�|�@�K�@�+@��@�"�@�+@�;d@�+@�
=@��y@��+@�=q@��#@���@���@��h@��7@��-@���@��^@��h@�p�@���@�Q�@��F@�|�@�33@�@�33@�C�@���@�^5@�?}@�Ĝ@��m@�"�@��!@�~�@�M�@�{@��^@���@��7@���@�{@�=q@�^5@�^5@�5?@�J@��#@�@���@��h@�p�@�7L@���@��@�r�@�Z@�9X@��;@��@�dZ@�"�@�
=@���@�ff@�-@��@���@���@���@��@�p�@�7L@�&�@�&�@�/@�7L@�G�@�p�@��@�/@�V@�V@�V@��@��@�V@��/@�Z@�  @��F@���@��@�dZ@�@���@�ff@�{@��#@���@�hs@�?}@��@��@��9@�j@�I�@�(�@�  @��@��m@�ƨ@��@�|�@�\)@�K�@�
=@���@��!@��\@�E�@��@��@�{@�{@���@��^@��h@�G�@��9@�9X@��m@��;@���@�l�@�+@�o@��R@���@�^5@�E�@�=q@�$�@�$�@�J@��7@�O�@�&�@��`@���@�Ĝ@��D@�j@���@��@�|�@�dZG�O�@��0@�e�@z�@s'�@i�@a�C@W�@O�@Io @C�W@<A�@6�H@.�@)��@!@@<6@s�@��@M@&�@�I111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBBBBBB%B1B
=BDBPBbBoB�B!�B=qB<jBO�Bs�By�B��B��B�'B�!B�FB�RB�B�B�B�B�B�B�B�B�B�-B�3B�9B�FB�FB�FB�?B�9B�9B�3B�?B�9B�9B�FB�?B�?B�?B�?B�?B�?B�FB�LB�LB�RB�LB�9B�B��B�7Bo�Be`BZBN�B:^B#�BB�fB�B��BĜB��B�dBBB�B��B�1B{�Bk�BS�B@�B-B�B�BVB+B  B
��B
�)B
��B
�9B
�B
��B
��B
��B
�%B
� B
|�B
x�B
k�B
P�B
K�B
>wB
�B
oB	�B	�5B	�B	��B	ÖB	�LB	�B	��B	��B	��B	��B	��B	��B	�7B	|�B	t�B	p�B	k�B	bNB	N�B	E�B	9XB	49B	.B	'�B	!�B	�B	{B	hB	
=B��B�B�B�sB�NB�HB�HB�B�B��B��B��BƨB�qB�XB�RB�FB�9B�!B��B��B��B��B��B��B��B��B��B��B�{B�bB�PB�=B�1B�%B�B�B� B}�B|�B{�By�By�By�Bx�Bw�Bv�Bu�Bt�Bt�Bs�Bs�Br�Bq�Bp�Bp�Bp�Bo�Bn�Bl�Bk�BjBgmBe`BaHB\)BYBVBVBW
BVBVBVBVBVBW
B[#B[#B\)B]/B]/B`BBcTBcTBe`BffBdZBcTBcTBaHB`BB_;B^5B^5B]/B]/B_;B`BB`BB`BBbNBcTBffBk�Bo�Bo�Bn�BhsBhsBiyBiyBo�Bq�Bp�Bq�Bv�Bu�Bx�B|�B|�B~�B� B� B�B�oB��B��B�B�3B�?B�RB�dB�qB�wB��BĜBȴBɺB��B��B��B�B�B�B�B�B�B�#B�HB�sB�B�B�B�B�B�B�B�B�B�B�B�B��B��B	  B	  B	B	B	B	%B	+B	
=B	
=B	DB	PB	\B	\B	hB	uB	�B	�B	�B	�B	�B	�B	"�B	#�B	&�B	(�B	+B	.B	1'B	49B	8RB	;dB	?}B	@�B	D�B	H�B	O�B	Q�B	T�B	XB	\)B	\)B	^5B	`BB	ffB	l�B	q�B	s�B	v�B	y�B	~�B	�B	�B	�+B	�1B	�=B	�VB	�PB	�PB	�oB	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�FB	�LB	�RB	�XB	�^B	�dB	�jB	�qB	�}B	��B	��B	��B	��B	ÖB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�BB	�TB	�fB	�fB	�`B	�`B	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
�B
kB
#�B
+�B
4B
:*B
CB
HfB
L0B
MjB
U�B
[�B
a�B
f�B
mCB
q[B
t�B
v�B
yXB
z�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B �B�B�B�B�B)BUB4 B2�BFlBj?BpbB�"B�zB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�DB�Bf+B[�BP�BEhB0�BnB��B�BлB�fB�:B�'B��B�0B�.B��B�6B~�Br�Bb)BJ�B7)B#�BaB4B�B
��B
��B
�oB
��B
�:B
��B
��B
��B
��B
�@B
|�B
v�B
s�B
o�B
b=B
G�B
B�B
57B
�B
	3B	�_B	��B	��B	B	�`B	�B	��B	��B	��B	��B	��B	��B	�WB	�B	s�B	k�B	guB	bYB	Y"B	E�B	<xB	0/B	+B	$�B	�B	�B	wB	VB	BB	B�B�B�uB�QB�.B�(B�(B��B��B��BĶB��B��B�VB�>B�8B�*B�B�B��B��B��B��B��B��B��B��B��B�{B�bB�GB�;B�'BB}B{Bx�Bv�Bt�Bs�Br�Bp�Bp�Bp�Bo�Bn�Bm�Bl�Bk�Bk�Bj�Bj�Bi�Bh�Bg�Bg�Bg�Bf�Be�BcyBbwBakB^[B\PBX8BSBPBL�BL�BM�BL�BL�BL�BL�BL�BM�BRBRBSBT BTBW6BZDBZEB\QB]YB[IBZFBZFBX7BW5BV.BU+BU$BT"BT#BV-BW6BW6BW4BY=BZGB]VBbzBf�Bf�Be�B_iB_cB`oB`kBf�Bh�Bg�Bh�Bm�Bl�Bo�Bs�Bs�Bu�Bv�Bv�Bw�B�bB��B��B� B�$B�.B�AB�SB�dB�fB�mB��B��B��BýB��B��B��B�B�	B�
B�	B�B�B�4B�_B�kB�B�}B�}B�B�B�B�B�B�B�B�B��B��B��B��B��B��B�B�B�B	%B	)B	/B	:B	HB	EB	QB	
_B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	(B	+#B	/;B	2KB	6bB	7gB	;�B	?�B	F�B	H�B	K�B	N�B	SB	SB	UB	W&B	]IB	cpB	h�B	j�B	m�B	p�B	u�B	x�B	z�B	~B	B	� B	�8B	�4B	�4B	�QB	�bB	�pB	��B	��B	��B	��B	��B	��B	�B	�B	�%B	�/B	�2B	�7B	�?B	�DB	�JB	�PB	�[B	�cB	�dB	�gB	�hB	�xB	�zB	��B	��B	��B	��B	ìB	ŸB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�%B	�%B	�#B	�&B	�"B	�%B	� B	�0B	�AB	�EB	�=B	�>B	�IB	�GB	�HB	�LB	�UB	�WB	�fB	�uB	�yB	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�	B	�	B
 B
 B
B
B
B
!G�O�B
B
DB
wB
"vB
*�B
1B
9�B
?CB
C	B
DBB
L�B
R�B
X�B
]\B
dB
h3B
k\B
m�B
p0B
q�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.009(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940282019060409402820190604094028  AO  ARCAADJP                                                                    20170630070102    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170630070102  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170630070102  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094028  IP                  G�O�G�O�G�O�                