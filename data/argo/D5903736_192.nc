CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-08-10T17:02:22Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20170810170222  20190604094029  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�T����1   @�UP���@6r� Ĝ�d����F1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A���B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dyd{D��D�\{D���D��=D�
D�T�D��=D��D�HD�8�D�|�DǹHD�
D�2�Dڣ3D�� D���D�EqD�HD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @z�H@�p�@�p�A�RA>�RA^�RA~�RA�\)A��\A�\)A�\)A�\)A�\)A�(�B zB�B�B�B�B'�B/�B7�B?�BG�BO�BXzB`zBgG�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C C"C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CVCW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#�{D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DI�GDI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��DtZ�Dy_\D�)D�Y�D���D���D�{D�R>D���D�ڐD���D�6gD�z>DǶ�D�{D�0 Dڠ�D��qD���D�B�D�~�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�FA�^A�jA�ĜA�jA�ĜA�ȴA���A���A���A���A���A���A���A���A�ƨA��A���A��A�~�A�XA�5?Aߣ�A���Aݝ�A�/Aڴ9A�-A�M�A�t�A�A�1'AП�Aϝ�A�z�AˑhA�Q�Aɏ\Aș�AƬAōPA��A��AēuA���A��HA���A�E�A��^A�^5A��A�`BA���A�{A���A���A��A��A�t�A��DA�ZA�ĜA�\)A�O�A��A���A��A�oA���A�hsA��A��A���A�;dA��yA�x�A�"�A�  A�;dA�M�A�p�A��`A��!A��\A�9XA�?}A�C�A���A�dZA���A���A�z�A�t�A�ZA��A���A�1'A���A�n�A��A���A�ȴA���A�?}A�l�A�ZA��#A���A�5?A�ffA���A�A}K�Azv�Av5?AtjAq�hAp  An~�Al��Ak�Aj1'AiS�Ah^5Ag�Ag�hAg%Af5?AedZAd�\Abr�A`�RA_ƨA_+A^  A\Q�AZbNAYG�AU�FARJAQ%AP�AN-AMx�AL �AJ9XAIAH��AF�HADn�ADJAC?}AA;dA?/A=�A<ĜA<M�A;�wA:�!A9p�A81'A5XA4�\A4M�A4bA3`BA2�A2 �A1hsA0r�A0bA/�^A/7LA.z�A,�yA,z�A+p�A*�A( �A'XA&��A&A"��A �9A��AƨA��A�+A��AA�
A�AE�AbA��A��A5?A��A��A�FAbNAJA��A$�A��A�A/Ar�AXA
ĜA
z�A	XA�!A�A�A�DAbA�FA��AhsA9XA~�A  AK�A v�@��@���@��@��;@���@��@���@���@�@��@��9@�Z@��m@��@�\)@��@�O�@�&�@��@�!@�$�@�V@���@�n�@�z�@�E�@�7L@�Q�@�Z@�r�@�@�E�@�-@�h@�A�@�@�@�=q@�X@�"�@թ�@� �@�  @�C�@҇+@�&�@�t�@�5?@�x�@ͩ�@�%@�1@���@�J@ȴ9@�o@Ƨ�@�$�@���@��#@�O�@�Q�@öF@�@�v�@�`B@�j@�~�@��T@�p�@���@�bN@��H@�^5@�`B@�r�@� �@���@�S�@�+@�@��R@�^5@�`B@���@��@�"�@���@��@���@�1'@��m@���@�x�@���@�Ĝ@�I�@�9X@���@�t�@��@���@�-@�{@�{@�J@��@��T@�@�?}@��/@���@�v�@��#@�x�@�r�@�ƨ@��w@��@�1@���@�C�@��H@�n�@���@�x�@�/@���@���@���@��@�z�@�Q�@��w@�33@�o@�
=@��@�E�@���@��T@��T@��T@�=q@�V@�5?@�5?@�@�&�@��@��@�p�@���@�x�@��@�/@�G�@�&�@�&�@�V@���@���@��j@��@�1@�|�@��P@��P@�K�@���@��\@�E�@���@���@��@��u@�I�@�9X@�  @� �@�b@��@�|�@�"�@�@��@�ȴ@��R@���@�V@�^5@�M�@��@���@�@��@��#@��T@�p�@�O�@�&�@���@�Ĝ@�z�@�Z@�I�@�A�@�A�@�A�@�z�@��@��D@�bN@�b@��@�|�@��y@���@���@�~�@��@��-@�?}@��@���@���@���@���@���@���@�%@���@���@��`@��`@��/@��j@��u@�I�@��
@�33@��@��H@��!@�ȴ@���@��\@�^5@�M�@�E�@�5?@�$�@�-@�5?@��@�{@��@���@�O�@�?}@�/@�&�@�V@�K^@��*@~+k@o��@g�@^�h@X�E@N��@IQ�@C33@;�@2�@)�o@#�:@�M@�@�@.I@	l@
��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�FA�^A�jA�ĜA�jA�ĜA�ȴA���A���A���A���A���A���A���A���A�ƨA��A���A��A�~�A�XA�5?Aߣ�A���Aݝ�A�/Aڴ9A�-A�M�A�t�A�A�1'AП�Aϝ�A�z�AˑhA�Q�Aɏ\Aș�AƬAōPA��A��AēuA���A��HA���A�E�A��^A�^5A��A�`BA���A�{A���A���A��A��A�t�A��DA�ZA�ĜA�\)A�O�A��A���A��A�oA���A�hsA��A��A���A�;dA��yA�x�A�"�A�  A�;dA�M�A�p�A��`A��!A��\A�9XA�?}A�C�A���A�dZA���A���A�z�A�t�A�ZA��A���A�1'A���A�n�A��A���A�ȴA���A�?}A�l�A�ZA��#A���A�5?A�ffA���A�A}K�Azv�Av5?AtjAq�hAp  An~�Al��Ak�Aj1'AiS�Ah^5Ag�Ag�hAg%Af5?AedZAd�\Abr�A`�RA_ƨA_+A^  A\Q�AZbNAYG�AU�FARJAQ%AP�AN-AMx�AL �AJ9XAIAH��AF�HADn�ADJAC?}AA;dA?/A=�A<ĜA<M�A;�wA:�!A9p�A81'A5XA4�\A4M�A4bA3`BA2�A2 �A1hsA0r�A0bA/�^A/7LA.z�A,�yA,z�A+p�A*�A( �A'XA&��A&A"��A �9A��AƨA��A�+A��AA�
A�AE�AbA��A��A5?A��A��A�FAbNAJA��A$�A��A�A/Ar�AXA
ĜA
z�A	XA�!A�A�A�DAbA�FA��AhsA9XA~�A  AK�A v�@��@���@��@��;@���@��@���@���@�@��@��9@�Z@��m@��@�\)@��@�O�@�&�@��@�!@�$�@�V@���@�n�@�z�@�E�@�7L@�Q�@�Z@�r�@�@�E�@�-@�h@�A�@�@�@�=q@�X@�"�@թ�@� �@�  @�C�@҇+@�&�@�t�@�5?@�x�@ͩ�@�%@�1@���@�J@ȴ9@�o@Ƨ�@�$�@���@��#@�O�@�Q�@öF@�@�v�@�`B@�j@�~�@��T@�p�@���@�bN@��H@�^5@�`B@�r�@� �@���@�S�@�+@�@��R@�^5@�`B@���@��@�"�@���@��@���@�1'@��m@���@�x�@���@�Ĝ@�I�@�9X@���@�t�@��@���@�-@�{@�{@�J@��@��T@�@�?}@��/@���@�v�@��#@�x�@�r�@�ƨ@��w@��@�1@���@�C�@��H@�n�@���@�x�@�/@���@���@���@��@�z�@�Q�@��w@�33@�o@�
=@��@�E�@���@��T@��T@��T@�=q@�V@�5?@�5?@�@�&�@��@��@�p�@���@�x�@��@�/@�G�@�&�@�&�@�V@���@���@��j@��@�1@�|�@��P@��P@�K�@���@��\@�E�@���@���@��@��u@�I�@�9X@�  @� �@�b@��@�|�@�"�@�@��@�ȴ@��R@���@�V@�^5@�M�@��@���@�@��@��#@��T@�p�@�O�@�&�@���@�Ĝ@�z�@�Z@�I�@�A�@�A�@�A�@�z�@��@��D@�bN@�b@��@�|�@��y@���@���@�~�@��@��-@�?}@��@���@���@���@���@���@���@�%@���@���@��`@��`@��/@��j@��u@�I�@��
@�33@��@��H@��!@�ȴ@���@��\@�^5@�M�@�E�@�5?@�$�@�-@�5?@��@�{@��@���@�O�@�?}@�/@�&�G�O�@�K^@��*@~+k@o��@g�@^�h@X�E@N��@IQ�@C33@;�@2�@)�o@#�:@�M@�@�@.I@	l@
��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�BVBcTBjB�bB��B�B�LBǮB��B��BɺBB�dB�3B��B��B��B��B�B�B�'B�-B�3B�B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�\B�\B�JB�7B�B{�Bt�Bo�BiyBW
BM�BH�B?}B5?B"�BVB1BB�B�B�fB�BB�#B��BÖB�B��B�B\)BG�B.BJB
�BB
�LB
��B
�%B
{�B
r�B
ffB
VB
C�B
+B
�B
PB
B	��B	�B	�TB	�)B	�
B	��B	��B	��B	ǮB	B	�dB	�FB	��B	��B	��B	��B	�\B	�B	u�B	l�B	N�B	<jB	7LB	49B	/B	/B	,B	%�B	"�B	�B	�B	�B	{B	\B	+B��B��B��B�B�B�sB�NB�/B��B��B��B��B��B��B��BǮBĜBĜBÖBÖBÖB�}B�jB�FB�'B�-B�9B�-B�B��B��B��B��B��B��B��B��B��B�PB�DB�uB��B�uB�bB�VB�=B�B� By�Bv�Bt�Bt�Bt�Bs�Bq�Bo�Bm�Bk�BiyBgmBgmBffBiyBs�Bv�Bu�Bt�Bp�Bl�Bm�BjBe`BbNBaHBaHBcTBbNBaHB`BBaHBbNBdZBffBgmBgmBgmBhsBhsBiyBl�Bl�Bl�Bk�Bk�Bk�BiyBhsBgmBffBffBffBe`Be`Be`Be`BdZBdZBdZBffBdZBe`BdZBe`BgmBgmBk�Bw�By�B{�B{�B~�B�B�DB�DB�DB�PB�\B�hB�oB��B��B��B��B��B��B��B��B��B�B�9B�XB�dB�qB�wBƨBǮB��B��B��B��B�
B�B�B�B�B�/B�TB�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	%B		7B	
=B	
=B	DB	DB	DB	DB	DB	JB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	(�B	+B	+B	+B	,B	.B	0!B	2-B	6FB	7LB	9XB	=qB	A�B	E�B	J�B	M�B	O�B	P�B	P�B	W
B	ZB	ZB	]/B	_;B	cTB	gmB	ffB	ffB	hsB	k�B	n�B	o�B	o�B	r�B	|�B	� B	�B	�B	�B	�%B	�%B	�1B	�1B	�=B	�\B	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�3B	�?B	�FB	�?B	�?B	�LB	�jB	�qB	�qB	�jB	�qB	�wB	�}B	B	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	�B	�
B	�
B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�/B	�BB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
9B
�B
�B
#�B
+B
8�B
;�B
AUB
G�B
M�B
TaB
T�B
[�B
b�B
h>B
m�B
p;B
vB
x8B
|6B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Bv�Bv�Bv�Bv�Bw�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bw�Bw�Bw�Bw�Bw�Bw�Bx�Bw�Bx�Bz�B��B� B8BK�BX�B`B��B�fB��B��B�IB�WB�pB�LB�'B��B��B��B�xB��B��B��B��B��B��B��B��B��B�bB�JB�tB��B��B��B��B��B��B��B��B��B��B�xB��B��B��B��B�xB�pB�WB�(B�B��B��B��B~�Bw�Bq�BjbBeCB_BL�BC{B>\B5#B*�B�BB��B��B�aB�;B�B��B��BʳB�IB��B�NBw�BQ�B=qB#�BB
�B
�B
�[B
{�B
q�B
h�B
\<B
K�B
9pB
 �B
�B
-B	��B	��B	�nB	�9B	�B	��B	��B	��B	´B	��B	�qB	�JB	�0B	��B	��B	��B	�vB	�FB	x�B	k�B	b{B	D�B	2WB	-?B	*)B	%B	%B	!�B	�B	�B	�B	�B	�B	
qB	SB�#B��B��B�B�B�B�lB�IB�*B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�nB�HB�*B�2B�=B�1B�B��B��B��B��B��B��B��B��B��B�VB�LB�zB��B�|B�lB�\B�DB{)BvBo�Bl�Bj�Bj�Bj�Bi�Bg�Be�Bc�Ba�B_�B]yB]xB\tB_�Bi�Bl�Bk�Bj�Bf�Bb�Bc�B`�B[mBX[BWVBWVBYcBX^BWUBVVBWWBX\BZgB\uB]{B]~B]}B^�B^�B_�Bb�Bb�Bb�Ba�Ba�Ba�B_�B^�B]{B\tB\xB\xB[qB[pB[oB[pBZkBZiBZlB\xBZjB[qBZgB[oB]�B]}Ba�Bm�Bo�Bq�Bq�BuB{/B�QB�RB�QB�_B�hB�tB�|B��B��B��B��B��B��B��B��B�B�B�GB�eB�pB�}B��B��B��B��B��B��B�B�B�B�B�B�B�:B�`B�B�B�B�B�B�B��B��B��B��B��B��B�	B�B�"B�"B�.B�AB	 FB	 HB	NB	JB	LB	KB	LB	QB		|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!B	!B	!B	"B	$B	&'B	(3B	,JB	-QB	/\B	3sB	7�B	;�B	@�B	C�B	E�B	F�B	F�B	MB	P"B	P B	S2B	U>B	YTB	]kB	\iB	\gB	^uB	a�B	d�B	e�B	e�B	h�B	r�B	u�B	yB	zB	{!B	|&B	|$B	~2B	~0B	�=B	�]B	�bB	�`B	�uB	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�0B	�=B	�FB	�=B	�<B	�DB	�gB	�nB	�nB	�gB	�oB	�rB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�$B	�<B	�AB	�FB	�FB	�FB	�LB	�LB	�LB	�UB	�QB	�SB	�SB	�RB	�YB	�[B	�dB	�tB	�wB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�.B
�B
�B
�B
 �B
.}B
1�B
7JB
=�B
C~B
JVB
J�B
Q�B
X�B
^3B
c�B
f0B
lB
n/B
r(B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.01(+/-0.001) in PSS-78.                                                                                                                                                                                             Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940292019060409402920190604094029  AO  ARCAADJP                                                                    20170810170222    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170810170222  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170810170222  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094029  IP                  G�O�G�O�G�O�                