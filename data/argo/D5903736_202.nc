CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-11-21T18:02:11Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20171121180211  20190604094031  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�7���81   @�7Z�u@5O\(��d�hr� �1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�RD�D�"�D���D�ƸD�
=D�4)D�6D�� D���D�I�D��
D�ȤD���D�6Dڏ\D�ӅD��D�>fD�p�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dt{D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt�GDy�3D��D� RD��HD��)D��D�1�D�3�D��qD��4D�G]D��{D��D��gD�3�Dڌ�D���D�D�;�D�nD��4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�AӁA�r�A�|�AӉ7AӍPAӋDAӑhAӑhAӓuAӑhAӑhAӓuAӕ�Aӗ�Aӛ�Aӝ�Aә�Aӛ�Aӛ�Aӝ�Aӣ�Aӡ�Aӟ�Aӟ�Aӛ�Aә�Aӛ�AӑhA�|�A�bNA�\)A�ZA�C�A��/AѓuA�z�A���A��A�7LAɑhA��/A�ZA���AìA�JA�33A��A�=qA�$�A�9XA�%A��/A��A���A�=qA���A�x�A�5?A���A��`A��
A���A��A�;dA��A��DA�5?A�=qA�1A��uA��^A�p�A��A��A���A�ffA�C�A�$�A��yA�-A�;dA�I�A�|�A��A�5?A��A�I�A��9A��A��!A���A��A�K�A��RA�E�A�-A���A�33A��A��mA�`BA���A�bNA��A���A���A�"�A��A�v�A�E�A��7A�  A�|�A}Ay|�AwƨAv�+Au�#Aut�At�/AtA�ArA�Aq�PAn�9Al�DAkS�Aj��Ai�wAh�yAg�mAd1Aa�FA]�TA\��A\n�A[��AY�hAV1AU;dASS�AR=qARAQ�7AO?}ANv�AM��AM
=AK�-AK&�AJ1'AH��AG��AE�AD�AD=qAC�AB{A@�yA?��A>�A=;dA<9XA:ZA8M�A8A7�A7�#A7��A7%A4�/A4�A3��A3oA2bNA1�mA1;dA0E�A/x�A.�A.��A.v�A. �A-��A-hsA,I�A*�/A)`BA(�jA'�A&E�A%�mA%oA$1'A#�A!�TA!A =qA�A�TA`BA��A�/A%An�A�#A+A��A1'A�`AoA?}At�AM�A33AC�A	��A�A��A��A�A%A��AZA"�A�RA�TA ��A @��R@�x�@�v�@���@��@��m@�;d@�p�@���@�^5@�j@�@�~�@� �@�^5@��@�@�ff@�I�@�n�@�-@�\)@��@ܼj@ۥ�@��@��@�  @�o@ա�@��;@���@�v�@�5?@ѩ�@��@�Z@��
@϶F@��@̬@ʰ!@�X@�+@�&�@�1'@���@å�@Õ�@Å@�ȴ@�~�@��^@�r�@��
@���@��@�O�@�?}@��j@��m@�t�@��H@��@��j@��;@���@�{@�`B@�l�@���@�^5@���@��@�A�@�ƨ@�t�@�M�@��T@���@�x�@�G�@��@��@���@��@�bN@�C�@��R@�V@��@��^@�`B@���@���@��@�bN@�(�@��;@��w@�S�@��y@��+@�$�@���@�p�@�G�@���@��@�b@�C�@�
=@�
=@��y@�ff@�$�@���@��@��#@��-@�hs@��`@��9@��D@�Z@�9X@�b@��;@���@�K�@��!@�@��^@��@�O�@�Ĝ@�Q�@��;@�|�@�C�@�;d@�;d@�+@���@��y@��@��!@���@�ff@�$�@��-@�p�@�/@��`@���@��9@��@��@��@�z�@��@���@�+@��H@���@�~�@�V@�E�@�{@�@���@�p�@�&�@���@�z�@�Z@�1'@�  @��w@���@��@��@��+@�-@��@�x�@�&�@��9@� �@���@�dZ@�
=@��H@�ff@�V@�n�@�@�X@�O�@�&�@��/@���@���@��/@��/@�%@��@��@�&�@�hs@��7@�x�@�O�@�7L@��@��@��@��@�r�@�Q�@� �@��
@��w@��@�|�@�dZ@�
=@��@��!@�=q@���@���@�G�@��@��@�V@���@��u@�bN@�A�@��
@��@�|�@�@��y@��y@��!@�v�@�M�@�=q@�{@��#@��-@��@�&�@�Ĝ@���@��@��@��@���@}�@yzx@q�@jkQ@b3�@Y��@Rߤ@J{�@D�?@=-w@5�@.s�@)k�@%�3@!�@��@ \@g8@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�z�AӁA�r�A�|�AӉ7AӍPAӋDAӑhAӑhAӓuAӑhAӑhAӓuAӕ�Aӗ�Aӛ�Aӝ�Aә�Aӛ�Aӛ�Aӝ�Aӣ�Aӡ�Aӟ�Aӟ�Aӛ�Aә�Aӛ�AӑhA�|�A�bNA�\)A�ZA�C�A��/AѓuA�z�A���A��A�7LAɑhA��/A�ZA���AìA�JA�33A��A�=qA�$�A�9XA�%A��/A��A���A�=qA���A�x�A�5?A���A��`A��
A���A��A�;dA��A��DA�5?A�=qA�1A��uA��^A�p�A��A��A���A�ffA�C�A�$�A��yA�-A�;dA�I�A�|�A��A�5?A��A�I�A��9A��A��!A���A��A�K�A��RA�E�A�-A���A�33A��A��mA�`BA���A�bNA��A���A���A�"�A��A�v�A�E�A��7A�  A�|�A}Ay|�AwƨAv�+Au�#Aut�At�/AtA�ArA�Aq�PAn�9Al�DAkS�Aj��Ai�wAh�yAg�mAd1Aa�FA]�TA\��A\n�A[��AY�hAV1AU;dASS�AR=qARAQ�7AO?}ANv�AM��AM
=AK�-AK&�AJ1'AH��AG��AE�AD�AD=qAC�AB{A@�yA?��A>�A=;dA<9XA:ZA8M�A8A7�A7�#A7��A7%A4�/A4�A3��A3oA2bNA1�mA1;dA0E�A/x�A.�A.��A.v�A. �A-��A-hsA,I�A*�/A)`BA(�jA'�A&E�A%�mA%oA$1'A#�A!�TA!A =qA�A�TA`BA��A�/A%An�A�#A+A��A1'A�`AoA?}At�AM�A33AC�A	��A�A��A��A�A%A��AZA"�A�RA�TA ��A @��R@�x�@�v�@���@��@��m@�;d@�p�@���@�^5@�j@�@�~�@� �@�^5@��@�@�ff@�I�@�n�@�-@�\)@��@ܼj@ۥ�@��@��@�  @�o@ա�@��;@���@�v�@�5?@ѩ�@��@�Z@��
@϶F@��@̬@ʰ!@�X@�+@�&�@�1'@���@å�@Õ�@Å@�ȴ@�~�@��^@�r�@��
@���@��@�O�@�?}@��j@��m@�t�@��H@��@��j@��;@���@�{@�`B@�l�@���@�^5@���@��@�A�@�ƨ@�t�@�M�@��T@���@�x�@�G�@��@��@���@��@�bN@�C�@��R@�V@��@��^@�`B@���@���@��@�bN@�(�@��;@��w@�S�@��y@��+@�$�@���@�p�@�G�@���@��@�b@�C�@�
=@�
=@��y@�ff@�$�@���@��@��#@��-@�hs@��`@��9@��D@�Z@�9X@�b@��;@���@�K�@��!@�@��^@��@�O�@�Ĝ@�Q�@��;@�|�@�C�@�;d@�;d@�+@���@��y@��@��!@���@�ff@�$�@��-@�p�@�/@��`@���@��9@��@��@��@�z�@��@���@�+@��H@���@�~�@�V@�E�@�{@�@���@�p�@�&�@���@�z�@�Z@�1'@�  @��w@���@��@��@��+@�-@��@�x�@�&�@��9@� �@���@�dZ@�
=@��H@�ff@�V@�n�@�@�X@�O�@�&�@��/@���@���@��/@��/@�%@��@��@�&�@�hs@��7@�x�@�O�@�7L@��@��@��@��@�r�@�Q�@� �@��
@��w@��@�|�@�dZ@�
=@��@��!@�=q@���@���@�G�@��@��@�V@���@��u@�bN@�A�@��
@��@�|�@�@��y@��y@��!@�v�@�M�@�=q@�{@��#@��-@��@�&�@�Ĝ@���@��@��G�O�@���@}�@yzx@q�@jkQ@b3�@Y��@Rߤ@J{�@D�?@=-w@5�@.s�@)k�@%�3@!�@��@ \@g8@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�?B�9B�?B�?B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�?B�?B�?B�?B�FB�RB�RB�XB�^B�dB�wB��BBBÖBŢB��B�B5?BB�BA�BC�BR�BaHBl�Bl�B�B�uB��B�oB�\B��B�uB�uB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�By�Bs�Bq�Bp�Bn�BhsBdZB]/BR�BI�BB�B>wB5?B�B�BDB��B�B�B�;B�B��B�wB�9B�B��B��B��B�+By�Bo�BhsBbNBaHB`BB\)BJ�B�B  B
�B
��B
��B
�VB
w�B
aHB
I�B
?}B
6FB
1'B
-B
)�B
,B
 �B
�B
	7B	��B	�B	�`B	�BB	�#B	��B	�RB	��B	�+B	}�B	y�B	t�B	jB	XB	Q�B	F�B	>wB	<jB	8RB	-B	'�B	#�B	�B	�B	oB	JB	
=B	
=B	B��B��B�B�yB�`B�BB�#B�B�B��BɺB��B��B��B��B��BǮBÖB��B�jB�^B�XB�?B�-B�'B�!B�'B�'B�?B�FB�9B�3B�B��B��B��B��B��B��B��B�oB�\B�PB�7B�+B�B�B}�Bx�Bu�Bt�Bs�Br�Bp�Bn�Bk�BjBiyBhsBffBdZBcTBaHBbNBaHB`BB_;B_;B^5B]/B]/B\)B\)B]/B]/B]/B]/B`BBbNBdZBdZBdZBffBgmBgmBhsBgmBffBiyBjBjBjBl�Bp�Bq�Bq�Bw�Bx�By�Bz�Bz�B|�B|�B|�B~�B�B�%B�%B�+B�1B�=B�DB�VB�\B�\B��B��B��B��B�B�B�!B�!B�!B�!B�-B�3B�FB�^B�qB�qB�wBŢBŢBǮBɺB��B��B��B�B�5B�ZB�sB�B��B��B��B	  B	B	1B	DB	JB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	,B	0!B	33B	6FB	8RB	:^B	=qB	>wB	>wB	@�B	B�B	C�B	D�B	F�B	I�B	L�B	N�B	P�B	S�B	S�B	VB	YB	\)B	aHB	cTB	cTB	dZB	hsB	iyB	jB	k�B	k�B	l�B	n�B	p�B	q�B	r�B	r�B	s�B	t�B	u�B	v�B	x�B	y�B	|�B	~�B	�B	�B	�B	�+B	�7B	�DB	�JB	�JB	�JB	�JB	�PB	�PB	�VB	�VB	�VB	�\B	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�9B	�?B	�?B	�FB	�XB	�^B	�qB	�wB	��B	��B	B	ĜB	ƨB	ɺB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�ZB	�`B	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
	7B
	7B

=B
DB
JB
pB
KB
 �B
-B
5%B
;B
?�B
ESB
J�B
O�B
S�B
ZkB
a-B
fLB
iyB
m�B
r-B
v�B
yXB
�OB
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�VB�B(�B5�B4�B6�BFLBT�B_�B_�BwmB��B��B��B��B��B��B��B��B��B��B�B��B�B�B�/B�'B�"B�$B�B��B��B��B��B��BwpBm0BgBe
Bc�Ba�B[�BW�BP�BFPB=B5�B1�B(�BB
�B��B�NB�B��BҢB͆B�FB��B��B�yB�RB�:B�Bz�BmQBcB[�BU�BT�BS�BO�B>CB	B
�B
ɈB
��B
�'B
��B
kcB
T�B
=QB
3B
)�B
$�B
 �B
�B
�B
eB
HB	��B	�hB	�)B	�B	��B	��B	ȦB	��B	��B	z�B	q�B	m�B	hmB	^1B	K�B	E�B	:[B	2-B	0!B	,B	 �B	�B	�B	vB	
BB	*B	 B��B��B��B�B�B�eB�=B�"B�B��B��B��BèB�B��B��B��B��B��B�qB�aB�HB�1B�%B� B�B��B��B��B��B��B�B�B�B��B��B��B��B��B�|B�mB�YB�SB�<B�)B�B}Bz�Bw�Bu�Bq�Bl�Bi�Bh�Bg�Bf�BdzBbpB_XB^TB]NB\IBZ?BX0BW*BUBV$BUBTBSBSBRBQBQ	BPBPBQ
BQ
BQ	BQBTBV*BX7BX6BX5BZDB[HB[KB\MB[IBZAB]SB^ZB^\B^WB`hBd�Be�Be�Bk�Bl�Bm�Bn�Bn�Bp�Bp�Bp�Br�Bv�Bz By�B{B|B~B B�.B�3B�5B�nB��B��B��B��B��B��B��B��B��B�B�
B� B�6B�JB�FB�PB�zB�yB��B��B��B��B��B��B�	B�,B�FB�SB�B��B�B��B��B�B�B	 B	MB	\B	gB	oB	tB	{B	�B	�B	�B	�B	�B	#�B	' B	*B	,B	.*B	1@B	2EB	2GB	4QB	6YB	7eB	8hB	:wB	=�B	@�B	B�B	D�B	G�B	G�B	I�B	L�B	O�B	UB	WB	WB	X$B	\>B	]CB	^JB	_MB	_PB	`UB	baB	dlB	etB	fyB	fvB	g�B	h�B	i�B	j�B	l�B	m�B	p�B	r�B	t�B	u�B	x�B	z�B	|�B	B	�B	�B	�B	�B	�B	�B	� B	�B	�B	�%B	�)B	�;B	�CB	�HB	�VB	�VB	�YB	�_B	�dB	�dB	�oB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�6B	�;B	�IB	�JB	�OB	�\B	�kB	�|B	��B	B	âB	ŭB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�$B	�"B	�%B	�6B	�9B	�CB	�LB	�JB	�KB	�MB	�PB	�XB	�bB	�`B	�gB	�hB	�gB	�oB	�wB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� G�O�B
+B
B
�B
 �B
(�B
/8B
3�B
9B
>�B
C�B
G�B
N(B
T�B
ZB
]3B
a�B
e�B
j�B
mB
tB
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.012(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940312019060409403120190604094031  AO  ARCAADJP                                                                    20171121180211    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171121180211  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171121180211  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094031  IP                  G�O�G�O�G�O�                