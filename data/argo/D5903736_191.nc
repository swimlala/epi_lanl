CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-07-31T07:01:02Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20170731070102  20190604094029  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�Ȯu�I1   @��D�_�@5�t�j~��d��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@ffBH��BO��BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtFfDy�)D��D�:�D���D���D�fD�<{D�iHD��D� D�L�D�u�D��D� �D�B�Dڈ�D��{D��fD�4{D�p�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)B zB�B�B�B�B'�B/�B7�B@zBHz�BOG�BWG�B_�Bg�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�DGDz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D�{Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��DtAGDy�
D�D�8RD���D��>D��D�9�D�f�D�D�qD�J>D�s4D�˅D��D�@ DچgD���D���D�1�D�ngD��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�uA�&�A��A��A�hsA�\)A�XA�M�A�=qA�JA��mA߾wAߏ\A�XA�=qA�/A��A�A�|�Aݥ�Aܙ�A� �A��HA���A�K�A��A�ȴA�;dA���A��Aˉ7A��A�XA�  A���Aə�A�{A�n�AǶFAƾwA�bAżjAť�A�l�A��/AËDA�ƨA�33A�S�A��A�E�A�bA�t�A���A�p�A��7A�XA��HA��PA�`BA���A�/A�O�A��wA�hsA���A��A�I�A��A���A���A��#A�A���A�+A���A�5?A��`A��FA�G�A��hA�(�A��A�n�A��yA�+A�z�A�K�A���A�33A��A�%A��#A�9XA��A���A���A���A��A�I�A��A��7A��A��uA���A���A��-A�O�A��A��^A���A��A�A�A��wA��
A��DA��A�p�A���A�ĜA��AC�A~�DA}��Ay��AuO�ArE�Ap��Ap9XAnVAj1'Ah�`Ag�Ae��Ab�\A`bNA_p�A^��A^ZA]l�A[|�AV-ASC�AP�AN��AK�;AI�AH  AGƨAG�AGS�AFJAEO�AD��AC�
AB��A@ĜA?�A=XA:A8�uA7VA5x�A4A�A1�^A05?A.��A+ƨA*5?A)ƨA)��A)�PA)l�A(M�A& �A$��A#�A!��A ^5A bA��A��A(�A/A+AI�AAdZA��A��A��A�9A(�A��A �A1'A�A33A�AS�A�HAffA��A�hA
�A	\)A�yAz�A�An�AA7LAz�A�AK�AȴAv�A5?AAA;dA I�@��@�5?@��R@���@��/@��@�@�b@�l�@�!@�@�A�@�E�@�bN@���@�S�@���@�&�@�I�@�dZ@�"�@�v�@�`B@�z�@��;@��@ו�@���@ԣ�@�o@���@�  @���@�7L@�%@��
@�\)@���@��y@ʏ\@�{@�G�@�j@�  @Ǖ�@��@�V@�=q@�-@���@�7L@ļj@�j@þw@��@�v�@�=q@�V@���@��u@�z�@�bN@�I�@�A�@��@�+@�@���@�ff@�=q@��^@�&�@�z�@�1'@�dZ@�33@�"�@�@��@���@���@��7@��@��/@��@�j@�A�@� �@��
@�33@�V@��@��@�A�@���@��@��y@�9X@�33@���@�v�@���@�I�@�dZ@�o@��@��@�O�@��j@�9X@�C�@��T@�`B@��@��`@��j@��D@��m@���@�t�@��R@�ff@�`B@���@�b@���@�t�@��@���@��@�J@�x�@�X@�`B@�&�@��D@��@��@�\)@��@��@��!@���@�n�@�=q@�$�@�@���@�@��-@�G�@���@���@��D@��@�j@�Z@�Q�@�A�@�I�@�Z@�(�@�1@���@��@���@�dZ@�
=@�
=@��R@�^5@�M�@�E�@�E�@�E�@�=q@���@�@�@���@�`B@�O�@�/@�7L@�7L@�&�@��j@��9@���@��`@��@�(�@�b@���@�S�@�;d@�;d@�K�@�K�@�C�@�C�@�C�@�"�@�@��@���@�{@���@���@���@���@���@�X@�V@���@���@���@�z�@��@��D@��D@��D@��D@��D@��u@��u@��D@��@�z�@�9X@�1@�ƨ@��@���@�\)@�33@��y@��y@���@���@���@��@���@�`B@�?}@�/@��`@�r�@��
@�K�@�C�@�;d@�K�@�33@�o@�@���@��@��y@��y@���@�ȴ@���@���@��\@��d@��3@'�@tc�@o@O@dU2@Z�6@Rq�@K�@@D��@=�@6v�@/l�@&�@C@�R@�e@��@�b@��@	�z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A�uA�&�A��A��A�hsA�\)A�XA�M�A�=qA�JA��mA߾wAߏ\A�XA�=qA�/A��A�A�|�Aݥ�Aܙ�A� �A��HA���A�K�A��A�ȴA�;dA���A��Aˉ7A��A�XA�  A���Aə�A�{A�n�AǶFAƾwA�bAżjAť�A�l�A��/AËDA�ƨA�33A�S�A��A�E�A�bA�t�A���A�p�A��7A�XA��HA��PA�`BA���A�/A�O�A��wA�hsA���A��A�I�A��A���A���A��#A�A���A�+A���A�5?A��`A��FA�G�A��hA�(�A��A�n�A��yA�+A�z�A�K�A���A�33A��A�%A��#A�9XA��A���A���A���A��A�I�A��A��7A��A��uA���A���A��-A�O�A��A��^A���A��A�A�A��wA��
A��DA��A�p�A���A�ĜA��AC�A~�DA}��Ay��AuO�ArE�Ap��Ap9XAnVAj1'Ah�`Ag�Ae��Ab�\A`bNA_p�A^��A^ZA]l�A[|�AV-ASC�AP�AN��AK�;AI�AH  AGƨAG�AGS�AFJAEO�AD��AC�
AB��A@ĜA?�A=XA:A8�uA7VA5x�A4A�A1�^A05?A.��A+ƨA*5?A)ƨA)��A)�PA)l�A(M�A& �A$��A#�A!��A ^5A bA��A��A(�A/A+AI�AAdZA��A��A��A�9A(�A��A �A1'A�A33A�AS�A�HAffA��A�hA
�A	\)A�yAz�A�An�AA7LAz�A�AK�AȴAv�A5?AAA;dA I�@��@�5?@��R@���@��/@��@�@�b@�l�@�!@�@�A�@�E�@�bN@���@�S�@���@�&�@�I�@�dZ@�"�@�v�@�`B@�z�@��;@��@ו�@���@ԣ�@�o@���@�  @���@�7L@�%@��
@�\)@���@��y@ʏ\@�{@�G�@�j@�  @Ǖ�@��@�V@�=q@�-@���@�7L@ļj@�j@þw@��@�v�@�=q@�V@���@��u@�z�@�bN@�I�@�A�@��@�+@�@���@�ff@�=q@��^@�&�@�z�@�1'@�dZ@�33@�"�@�@��@���@���@��7@��@��/@��@�j@�A�@� �@��
@�33@�V@��@��@�A�@���@��@��y@�9X@�33@���@�v�@���@�I�@�dZ@�o@��@��@�O�@��j@�9X@�C�@��T@�`B@��@��`@��j@��D@��m@���@�t�@��R@�ff@�`B@���@�b@���@�t�@��@���@��@�J@�x�@�X@�`B@�&�@��D@��@��@�\)@��@��@��!@���@�n�@�=q@�$�@�@���@�@��-@�G�@���@���@��D@��@�j@�Z@�Q�@�A�@�I�@�Z@�(�@�1@���@��@���@�dZ@�
=@�
=@��R@�^5@�M�@�E�@�E�@�E�@�=q@���@�@�@���@�`B@�O�@�/@�7L@�7L@�&�@��j@��9@���@��`@��@�(�@�b@���@�S�@�;d@�;d@�K�@�K�@�C�@�C�@�C�@�"�@�@��@���@�{@���@���@���@���@���@�X@�V@���@���@���@�z�@��@��D@��D@��D@��D@��D@��u@��u@��D@��@�z�@�9X@�1@�ƨ@��@���@�\)@�33@��y@��y@���@���@���@��@���@�`B@�?}@�/@��`@�r�@��
@�K�@�C�@�;d@�K�@�33@�o@�@���@��@��y@��y@���@�ȴ@���@���G�O�@��d@��3@'�@tc�@o@O@dU2@Z�6@Rq�@K�@@D��@=�@6v�@/l�@&�@C@�R@�e@��@�b@��@	�z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
B
��B
��B
�}B
�wB
�qB
�qB
�jB
�jB
�dB
�^B
�jB
ÖB
ɺB
��B
�/B
�BB
�sBB?}B��B�B  B��B�B�`B�BB\B-BI�B[#BdZBiyBl�Bm�Bn�Bq�Bv�By�B�B�%B�7B�=B�DB�VB�oB��B��B��B�B�-B�FB�XB�XB�LB�?B�RB�XB�XB�RB�FB�?B�-B�FB�XB�LB�-B�!B�'B�B��B��B�B��B��B�PB�%Bx�BffBYBJ�BB�B@�B:^B49B,B�B�BuBB��B��B�yB�NB�5B�)B�#B�B�B��BƨB�^B�3B�B��B��B�bB�DB�+B� BjBR�B)�B
��B
�/B
ĜB
�^B
�B
��B
� B
ffB
aHB
\)B
R�B
;dB
%�B
{B
	7B
B	��B	�BB	�B	��B	�wB	�B	��B	��B	�uB	�VB	�%B	w�B	ZB	H�B	:^B	1'B	+B	$�B	�B	�B	�B	�B	�B	�B	{B	hB	DB	B��B�B�mB�BB�#B��B��BƨB��B�dB�LB�9B�-B�!B�B�B��B��B��B��B�{B�oB�hB�hB�\B�PB�7B�B�B�B�B� B� B}�B{�Bz�Bw�Br�Bo�Bn�Bm�Bk�BiyBiyBhsBgmBe`BdZBcTBbNB`BB_;B_;B^5B]/B]/B\)B[#B[#B[#BZBZBYBXBW
BS�BT�BW
BW
BVBXBXB[#B[#B[#B\)B]/B_;BaHBgmBiyBk�Bk�Bl�Bm�Bm�Bm�Bl�Bm�Bl�BiyBhsBiyBhsBiyBjBl�Bo�Bq�Bp�Bq�Bu�Bz�Bz�Bz�B|�B~�B�B� B� B�B�B�%B�+B�DB�bB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�-B�-B�3B�FB�^B�dB�wB�}B�}B��B��B��BĜBȴB��B��B��B��B��B��B��B��B��B�/B�/B�5B�;B�;B�BB�B�B�B�B��B��B	  B	B	B	B	+B	
=B	PB	hB	�B	�B	�B	�B	 �B	#�B	)�B	,B	1'B	33B	2-B	49B	;dB	@�B	D�B	F�B	I�B	N�B	T�B	T�B	VB	W
B	W
B	XB	\)B	_;B	_;B	`BB	dZB	hsB	iyB	jB	k�B	l�B	m�B	n�B	n�B	m�B	o�B	q�B	v�B	x�B	x�B	x�B	z�B	{�B	|�B	~�B	� B	� B	�B	�B	�B	�1B	�DB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�FB	�LB	�XB	�dB	�jB	�qB	��B	ÖB	ƨB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�)B	�5B	�;B	�;B	�BB	�BB	�NB	�TB	�`B	�fB	�mB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
fB
MB
�B
*�B
.cB
7�B
<�B
B�B
F�B
K�B
PHB
V�B
]�B
b4B
h
B
l"B
raB
u�B
y>B
|jB
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
��B
�~B
�zB
�wB
�mB
�eB
�fB
�]B
�`B
�]B
�UB
�bB
��B
��B
��B
�%B
�6B
�hB
�B5mB��B�uB��BøB��B�DB�B��B7B"�B?�BP�BZ7B_VBbcBcmBdoBg�Bl�Bo�Bv�B{�BB�B� B�-B�IB�_B��B��B��B�B�B�,B�/B�"B�B�,B�0B�1B�'B�B�B�B�!B�0B�#B�B��B�B��B��B��B��B��B�dB�-B|Bn�B\GBN�B@�B8pB6iB0AB*B!�B�B�B	_B��B��B�B�dB�9B�"B�B�B�B��B��B��B�SB�#B�B��B��B�UB�5B} Bu�B`vBH�B�B
��B
�5B
��B
�iB
� B
��B
vB
\yB
WZB
R9B
IB
1yB
�B

�B	�QB	�2B	��B	�_B	�"B	��B	��B	�*B	��B	��B	��B	�yB	|HB	m�B	PGB	>�B	0�B	'QB	!0B	B	�B	�B	�B	�B	�B	�B	
�B	�B	vB�=B�B��BݡB�wB�ZB�/B�B��B��B��B��B�sB�jB�\B�TB�JB�,B� B��B��B��B��B��B��B��B��BvB{^ByOBxJBwDBv>BvABt7Br,Bq BnBh�Be�Bd�Bc�Ba�B_�B_�B^�B]�B[�BZ�BY�BX�BV�BU�BU�BT}BSsBStBRoBQnBQkBQfBPhBPaBO`BNYBMNBJBBKDBMOBMRBLLBNVBNTBQkBQkBQiBRuBSuBU�BW�B]�B_�Ba�Ba�Bb�Bc�Bc�Bc�Bb�Bc�Bb�B_�B^�B_�B^�B_�B`�Bb�Be�Bg�Bf�Bg�BlBq,Bq*Bq)Bs6BuABwKBvGBvGBwLBzbB|mB}rB��B��B��B��B��B��B��B��B�B�B�%B�%B�)B�3B�3B�CB�YB�_B�qB�pB�pB�xB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�#B�!B�1B�>B�rB�rB�{B�}B�}BևB��B��B��B��B�B�+B�AB�GB�EB�UB�mB	 {B	�B	�B	�B	�B	�B	�B	B	B	 ;B	"GB	'gB	)nB	(kB	*yB	1�B	6�B	:�B	<�B	?�B	EB	K:B	K9B	LCB	MIB	MDB	NNB	RdB	UyB	UxB	V{B	Z�B	^�B	_�B	`�B	a�B	b�B	c�B	d�B	d�B	c�B	e�B	g�B	mB	oB	oB	oB	qB	r B	s&B	u3B	v8B	v:B	wBB	w@B	{ZB	~fB	�~B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�5B	�JB	�\B	�iB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�/B	�8B	�<B	�HB	�[B	�gB	�mB	�lB	�vB	�rB	�|B	نB	۔B	ܗB	ݠB	ݞB	ݞB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	�B	�B	�B	�+B	�$B	�&B	�,B	�*B	�2B	�-B	�8B	�8B	�4B	�;B	�8B	�6B	�=B	�<B	�<B	�>G�O�B	��B
|B
�B
 �B
$�B
-�B
2�B
8�B
=&B
A�B
FtB
L�B
S�B
X_B
^9B
bMB
h�B
k�B
okB
r�B
u&11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.01(+/-0.001) in PSS-78.                                                                                                                                                                                             Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940292019060409402920190604094029  AO  ARCAADJP                                                                    20170731070102    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170731070102  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170731070102  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094029  IP                  G�O�G�O�G�O�                