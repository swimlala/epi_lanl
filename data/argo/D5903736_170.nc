CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:55Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041155  20190604094025  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @����	�1   @��ݲ�*�@4	x����d�|�hs1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DHy�DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�
D��D�;�D�{�D��\D��D�HRD���D�ÅD�D�J=D��3D�˅D��D�5�D�FD��fD�{D�H D�z�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @4z�@z�H@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
B�
=B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C.C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DGGDGz�DG��DHt{DH��DI�GDI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO�{DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtt{Dy��D�RD�9HD�x�D���D� D�E�D��HD���D��D�G�D���D���D��D�34D�C�D���D��D�EqD�x D��g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��mA��yA��A��yA��A��A��`A��TA��HA��/A��`A��`A��mA��`A��`A��mA��mA��A��yA��A�A�A�
=A��A�{A��A��A��A�&�A�33A�C�Aћ�A�A��A��A��A���A���AыDA��yA�(�A�A�XA�9XA�S�Aʇ+Aɛ�A�p�A��Aţ�A�{A¶FAA�v�A�Q�A��mA�5?A�1'A�t�A�VA�7LA��A��;A��#A���A��^A�|�A��A�{A�ĜA��A���A��A�r�A��A��yA�ĜA��\A���A���A�A���A�`BA�9XA��A���A���A��A���A�~�A�+A��-A�JA���A��A���A���A�ĜA���A���A�  A�VA�M�A��-A�E�A�(�A�~�A��A���A��A�$�A��A�K�A��A��`A��A�oA{S�Awl�AvM�AtJAp{An=qAk��Ai�7AfE�Ad�Aa�A^�`A]�mAZ��AXJAV�!AUK�AS�hAR1AO�AO;dAM�AK�FAJ �AH��AHA�AHbAG�;AGt�AF�AE�;AE`BAD�\ACACx�AB��ABbA@�+A?�hA??}A>E�A<bA;O�A:~�A9��A9"�A8{A7A6^5A5+A3�A1��A09XA.z�A-XA+x�A)�
A(�`A'/A&A�A%��A%?}A#�^A"�DA"�A!��A �jA�A��A�A�A��AE�A��A��A33A�AZA�PAQ�A\)Ap�A��A�FA+A��A��A�jA��Ax�A�jA33A^5A
n�A(�A33A�!A�9A�DAQ�A�Ap�A�AA�yA��AQ�A  Al�AM�A��A/A b@�1@�(�@�x�@�=q@��@�Q�@���@�9@��@�ƨ@柾@�V@��T@�?}@�j@�(�@�l�@�@�@���@��
@�{@�X@�  @��H@�J@�7L@���@�S�@�E�@���@�Z@��m@ҸR@�V@�hs@Ѓ@ЋD@��@�t�@�o@θR@��@̋D@�l�@�
=@���@ɩ�@Ǿw@�{@ũ�@ċD@�1@�S�@��@�G�@��w@��@��\@���@���@�G�@��@�V@��`@���@��@�o@�~�@�{@�J@��@��@��@�x�@�V@���@�r�@���@�C�@���@��@�t�@�~�@�V@��@��7@�?}@��`@���@��R@�J@��h@�hs@�hs@�`B@�G�@��@�%@���@�|�@���@��7@��7@�x�@�O�@��9@��@�+@��+@�J@���@�O�@��@��u@�Q�@�9X@��
@�l�@��H@�^5@���@��@��7@�?}@�V@��/@��9@��u@�z�@�1@�b@�  @�1@�b@�1@���@�&�@�V@�V@�%@��`@��u@�9X@�1'@�z�@�  @��j@�Q�@�9X@�I�@�9X@�9X@� �@��@��@��@�1@��;@���@�  @�  @�  @��m@�@���@�=q@��@���@�@���@��h@��/@��@�Q�@���@���@�$�@�`B@��@�bN@�1'@� �@��;@���@��@�K�@�+@�o@���@�E�@��@���@��T@���@���@��^@��h@��@�O�@�7L@�/@�%@�z�@�Q�@�1'@�b@���@��
@��P@�33@�ȴ@�n�@�-@��#@���@��h@�x�@�O�@��@���@���@�I�@� �@���@��m@�|�@�33@��@���@�ff@�J@���@��^@��-@��@�O�@�7L@��@�%@��`@��@�Z@��@�S�@�
=@�@���@�=q@�J@��T@�@�x�@�?}@�/@��`@��D@�j@�b@��@�n/@x�5@o&@g�r@\M@TI�@N_@H�@A��@:�@4�@.ߤ@)��@"��@�@�@e�@��@t�@,=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��mA��yA��A��yA��A��A��`A��TA��HA��/A��`A��`A��mA��`A��`A��mA��mA��A��yA��A�A�A�
=A��A�{A��A��A��A�&�A�33A�C�Aћ�A�A��A��A��A���A���AыDA��yA�(�A�A�XA�9XA�S�Aʇ+Aɛ�A�p�A��Aţ�A�{A¶FAA�v�A�Q�A��mA�5?A�1'A�t�A�VA�7LA��A��;A��#A���A��^A�|�A��A�{A�ĜA��A���A��A�r�A��A��yA�ĜA��\A���A���A�A���A�`BA�9XA��A���A���A��A���A�~�A�+A��-A�JA���A��A���A���A�ĜA���A���A�  A�VA�M�A��-A�E�A�(�A�~�A��A���A��A�$�A��A�K�A��A��`A��A�oA{S�Awl�AvM�AtJAp{An=qAk��Ai�7AfE�Ad�Aa�A^�`A]�mAZ��AXJAV�!AUK�AS�hAR1AO�AO;dAM�AK�FAJ �AH��AHA�AHbAG�;AGt�AF�AE�;AE`BAD�\ACACx�AB��ABbA@�+A?�hA??}A>E�A<bA;O�A:~�A9��A9"�A8{A7A6^5A5+A3�A1��A09XA.z�A-XA+x�A)�
A(�`A'/A&A�A%��A%?}A#�^A"�DA"�A!��A �jA�A��A�A�A��AE�A��A��A33A�AZA�PAQ�A\)Ap�A��A�FA+A��A��A�jA��Ax�A�jA33A^5A
n�A(�A33A�!A�9A�DAQ�A�Ap�A�AA�yA��AQ�A  Al�AM�A��A/A b@�1@�(�@�x�@�=q@��@�Q�@���@�9@��@�ƨ@柾@�V@��T@�?}@�j@�(�@�l�@�@�@���@��
@�{@�X@�  @��H@�J@�7L@���@�S�@�E�@���@�Z@��m@ҸR@�V@�hs@Ѓ@ЋD@��@�t�@�o@θR@��@̋D@�l�@�
=@���@ɩ�@Ǿw@�{@ũ�@ċD@�1@�S�@��@�G�@��w@��@��\@���@���@�G�@��@�V@��`@���@��@�o@�~�@�{@�J@��@��@��@�x�@�V@���@�r�@���@�C�@���@��@�t�@�~�@�V@��@��7@�?}@��`@���@��R@�J@��h@�hs@�hs@�`B@�G�@��@�%@���@�|�@���@��7@��7@�x�@�O�@��9@��@�+@��+@�J@���@�O�@��@��u@�Q�@�9X@��
@�l�@��H@�^5@���@��@��7@�?}@�V@��/@��9@��u@�z�@�1@�b@�  @�1@�b@�1@���@�&�@�V@�V@�%@��`@��u@�9X@�1'@�z�@�  @��j@�Q�@�9X@�I�@�9X@�9X@� �@��@��@��@�1@��;@���@�  @�  @�  @��m@�@���@�=q@��@���@�@���@��h@��/@��@�Q�@���@���@�$�@�`B@��@�bN@�1'@� �@��;@���@��@�K�@�+@�o@���@�E�@��@���@��T@���@���@��^@��h@��@�O�@�7L@�/@�%@�z�@�Q�@�1'@�b@���@��
@��P@�33@�ȴ@�n�@�-@��#@���@��h@�x�@�O�@��@���@���@�I�@� �@���@��m@�|�@�33@��@���@�ff@�J@���@��^@��-@��@�O�@�7L@��@�%@��`@��@�Z@��@�S�@�
=@�@���@�=q@�J@��T@�@�x�@�?}@�/@��`@��D@�jG�O�@��@�n/@x�5@o&@g�r@\M@TI�@N_@H�@A��@:�@4�@.ߤ@)��@"��@�@�@e�@��@t�@,=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�!B
�3B
�dB
�XB
�XB
�dB
�^B
ÖB
ɺB
��B%BR�Bw�B� B�PB��B�9B��B�B�`B�B��BBbB#�B1'BG�BS�BW
BaHBk�Bk�BjBjBw�B��B��B�LB�}BȴB��B��B��B��B��B�'B��B��B��B��B��B��B��B��B��B��B��B��B�\B�Bx�Bn�BdZB\)BO�BE�B:^B5?B33B1'B,BbB�B�)BB�FB�FB��B��B��B�{B�Bn�B\)BL�B9XB5?B0!B#�B	7B
��B
�B
��B
�B
��B
�B
`BB
A�B
49B
 �B
+B	��B	�NB	��B	�^B	�B	��B	�7B	� B	n�B	`BB	XB	P�B	E�B	=qB	2-B	.B	(�B	�B	�B	oB	bB	\B	\B	JB	
=B	B	B��B��B��B��B�B�B�B�B�mB�`B�TB�NB�HB�;B�/B�)B�#B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBɺBɺBȴBȴBȴBǮBȴBȴBȴBȴBȴBǮBǮBǮBǮBƨBŢBĜBÖBÖBB��B��B�}B�wB�qB�jB�dB�FB�!B�B�B�B�B�!B�'B�'B�LB�LB�FB�?B�3B�!B�B��B��B��B��B�\B�%B�B�B�B�B|�Bw�Bs�Br�Bs�Br�Br�Bs�Bt�Bt�Bs�Bt�Bv�Bu�Bw�By�Bz�B|�B� B�B�B�B�B�B�+B�=B�=B�PB�uB��B��B��B��B�B�B��B��B��B�B�!B�B�-B�?B�FB�XB�}BƨB��B��B��B��B�B�/B�NB�mB�mB�fB�mB�B�B��B��B	+B	JB	PB	VB	VB	\B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	'�B	(�B	+B	-B	.B	/B	1'B	2-B	49B	5?B	5?B	5?B	6FB	6FB	6FB	5?B	49B	49B	49B	49B	5?B	5?B	6FB	8RB	;dB	=qB	?}B	@�B	B�B	C�B	D�B	E�B	F�B	H�B	L�B	N�B	N�B	P�B	P�B	R�B	S�B	VB	ZB	]/B	_;B	cTB	ffB	jB	m�B	r�B	x�B	� B	�%B	�PB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�LB	�dB	�}B	�}B	�}B	�}B	��B	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�5B	�BB	�HB	�TB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B

XB
&B
+B
%�B
3�B
;�B
CB
H1B
L0B
R�B
WsB
\�B
b�B
fB
lB
n�B
u?B
y$B
}<B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�"B
�B
�B
�%B
�B
�PB
�vB
εB �BM�Br�Bz�B�
B��B��B�sB��B�B�CB�yB��BB�B+�BBXBN�BQ�B[�Bf2Bf1Be1Be)Br|B�9B��B��B�(B�]B�{B�sB�vB�uBʇB��B��B�aB�<B�0B�.B�\B�nB�lB�oB�gB�PB�2B�B}�Bs�BiJB_	BV�BJ�B@QB5B/�B-�B+�B&�BB�`B��B�MB�B�B��B��B�~B�0B}�BiSBV�BG�B4B/�B*�B�B�B
�B
�QB
ɟB
��B
��B
|�B
[B
<SB
/B
�B
�B	��B	�B	ʱB	�0B	��B	�sB	�B	z�B	ilB	[B	R�B	K�B	@xB	8HB	-B	(�B	#�B	�B	nB	KB	<B	
8B	
5B	&B	B��B��B��B��B��B�B�B�nB�`B�\B�IB�@B�1B�+B�"B�B�B�B� B��B��B��BɺBȭBǪBȰBȱBȵBȴBǮBƥBŜBŢBŢBęBĚBěBÒBÖBÕBBÖBÒBÖB×BÓBBBBB��B��B�B�vB�vB�oB�kB�gB�ZB�ZB�SB�MB�@B�*B�B��B��B��B��B�B�	B�B�-B�0B�(B�#B�B�B��B��B��B��B��B�AB�	B~�B{�B}�B|�Bw�Br�Bn�Bm�Bn�Bm�Bm�Bn�Bo�Bo�Bn�Bo�Bq�Bp�Br�Bt�Bu�Bw�Bz�B B~�B|�B{�B}�B�B�$B�'B�7B�XB�lB�|B��B��B��B��B��B��B��B��B�B�B�B�(B�-B�>B�aB��BȸB��B��B��B�B�B�5B�QB�SB�KB�PB�dB�~B�B��B	B	.B	4B		9B		;B	
BB	fB	gB	vB	vB	xB	�B	�B	�B	�B	 �B	"�B	#�B	%�B	'�B	(�B	)�B	,
B	-B	/B	0"B	0 B	0"B	1,B	1+B	1$B	0%B	/B	/B	/B	/B	0#B	0"B	1-B	31B	6FB	8TB	:^B	;`B	=oB	>yB	?�B	@�B	A�B	C�B	G�B	I�B	I�B	K�B	K�B	M�B	N�B	P�B	T�B	XB	ZB	^5B	aHB	e_B	hrB	m�B	s�B	z�B	�B	�1B	�>B	�BB	�HB	�QB	�jB	�}B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�BB	�ZB	�\B	�VB	�^B	�iB	��B	ÔB	ĖB	ĘB	ŞB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ʼB	ȱB	ǪB	ɳB	ɵB	ɹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�&B	�/B	�5B	�<B	�@B	�JB	�MB	�SB	�VB	�VB	�\B	�bB	�bB	�mB	�tB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
2B
B
B
 pB
.�B
6uB
=�B
CB
GB
M�B
ROB
W�B
]]B
`�B
f�B
i�B
pB
s�B
xB
}�B
�|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.005(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940252019060409402520190604094025  AO  ARCAADJP                                                                    20181121041155    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041155  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041155  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094025  IP                  G�O�G�O�G�O�                