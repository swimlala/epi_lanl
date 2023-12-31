CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-04-17T15:38:28Z creation;2020-04-17T15:38:32Z conversion to V3.1;2022-11-21T05:27:14Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ],   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200417153828  20221123114512  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_207                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @��d�~�1   @��)�� @:�IQ����ds��,=1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�8 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�$�A��A� �A��^A��hA���A���A�bNA�=qA��A��-A�S�A�+A�%A��A��
A��uA�ffA��yA�t�A�9XA�ĜA��A�JA��9A�I�A���A���A���A���A�=qA��A�?}A��;A��wA���A�ZA�A��uA�G�A���A��A�jA���A�z�A�M�A� �A�ffA���A���A�S�A���A��A�;dA�1'A���A�oA�x�A�E�A��#A�M�A���A���A�|�A�x�A���A�/A��/A�jA��A�n�A�%A� �A�~�A��A���A�\)A��HA�M�A�^5A�7A~��A~I�A}�A|�+A|bNA|$�A{�-A{S�A{
=Ax~�Av{At��AtffAs�TAsdZAp��Ap5?Ao|�AmS�Ak�wAj�Ai��Ah��Ah1'Ag�Af��Af$�Ad��Ac�Ab�A`~�A_O�A^ȴA^5?A]��A]x�A]�A[�
AZ(�AW%AU33ASARbAN��AN�AO/AMdZAJ�!AJ{AI`BAH�AHZAG�#AGl�AF�AFAD��ADJACXAB�/AA��AAK�AA&�A@^5A?;dA=��A<��A<JA9�PA7��A7"�A5��A4��A4^5A4(�A3l�A3;dA2ffA1A1
=A05?A/��A.�+A.$�A-�A-+A,$�A+dZA)�A)l�A)7LA)�A(��A(�A'��A'+A&z�A&  A%�^A%�A%33A$��A$bA"�A �/A $�A?}A�A{A�^A`BA�A�A��A$�A�^A
=AI�A�Ax�A�\A��A{AhsAK�A33A�A��A�/A1'A�HA �A`BAK�A;dA33AAr�A/A�wAA�AC�A
�RA	�A	��A�A��A��AQ�A�
A�yAQ�A`BA�A ��@�ƨ@��-@��P@��7@��@��/@��@�9X@��@��@���@�E�@�7L@�(�@�S�@�
=@��@�ff@�-@���@�P@�=q@�7L@��@�$�@��`@��@睲@�~�@�7@�X@�%@�9@��
@�\)@�@�Ĝ@�|�@�^5@�p�@�Q�@���@�{@��
@���@ԣ�@ԃ@���@��@�+@́@�I�@ˮ@�v�@�&�@���@� �@��;@�t�@Ɨ�@���@ċD@�dZ@��#@�r�@���@�t�@�l�@�
=@��h@�bN@�S�@��y@��`@�;d@��@��@�C�@�~�@�p�@�1@���@���@�$�@�&�@��@�(�@���@�M�@���@���@�b@�o@�V@���@���@�dZ@�"�@��\@�V@�=q@��@�J@���@�%@��@��w@�l�@�o@���@�ff@�n�@�n�@�n�@�n�@�^5@�M�@�-@�J@�@�/@��D@�ƨ@��@��R@�V@�$�@�@���@��j@�j@��
@�o@�M�@���@�V@�r�@� �@��
@��@�ȴ@�M�@��^@��j@�Z@�  @��m@��P@���@�-@��^@���@��h@�G�@��@� �@��F@��P@�t�@�l�@�\)@�K�@�K�@�33@�o@���@�v�@���@�X@��@���@���@���@��9@���@��D@�1'@��@�K�@�+@�@��@���@��@���@�O�@�r�@�I�@�1'@��;@�S�@�o@���@�^5@�E�@�=q@�5?@�$�@��@��^@���@�G�@�V@��/@���@�Z@� �@�P@+@~�y@~ȴ@~��@~5?@~5?@~5?@~$�@}�@}`B@|��@|1@{��@{dZ@{C�@{@z�@z�!@y��@y��@x�`@w�P@w�@v�+@vff@vV@vE�@vE�@v$�@uO�@s�F@sdZ@s"�@r��@rn�@q��@q��@p�9@p  @oK�@o�@n��@m�-@l�j@lI�@l�@l�@k��@k"�@i��@i7L@h�@h  @g�w@g�P@gl�@g+@f��@f�@fȴ@f��@e�T@ep�@d�/@d�@dj@dI�@dI�@d9X@d(�@c�
@ct�@co@c@b��@b�\@bM�@a&�@`��@`bN@`  @_�P@^��@^V@]�T@]�@]?}@\��@\�@\j@\I�@\9X@[�m@[S�@Z�@Z�H@Z��@Z�@Y��@Y�^@Y7L@X��@XQ�@W�@W�P@W|�@Wl�@WK�@W+@W
=@W
=@W
=@V��@V�@Vff@U�@T�/@S�
@S�@SdZ@SC�@SC�@S33@S"�@So@S@So@R�@R��@Q��@Q�@PĜ@P�@PbN@P1'@P  @O�;@O�;@O�;@O�w@O��@Ol�@OK�@O
=@N�R@Nv�@N5?@M��@MO�@L�j@Lj@L9X@L1@K�m@K�@KdZ@KS�@K"�@J��@J�\@J=q@I�@I�^@IX@I�@H��@H��@H�u@H  @G�w@G|�@GK�@G+@F�y@Fȴ@F��@FV@F5?@F{@E��@E�-@E�h@E`B@E�@D��@D�/@Dj@D1@Cƨ@C��@CS�@B��@B�!@B��@B~�@B-@A��@A�@A�#@A�^@A��@A�7@AG�@A7L@A�@@�9@@Q�@@  @@  @@  @@  @@  @@b@?�;@?
=@>ȴ@>��@>ff@>V@>5?@=�T@=`B@<z�@<(�@<1@;�
@;�@;t�@;t�@;t�@;S�@;C�@;33@;@:��@:^5@9��@9��@9X@8�9@8r�@81'@7�;@7�P@7+@7
=@6�y@6�@6�@6ff@5�T@5@5��@5�@4��@4�j@4��@4z�@4�@3��@3�@3t�@3t�@3t�@3t�@3dZ@3S�@3S�@333@2M�@1��@1��@1��@1x�@17L@0��@0�9@0�@0 �@/\)@/K�@.��@.$�@-�h@-O�@-V@,��@,z�@,j@,Z@,9X@,9X@,(�@,�@,1@+�m@+�
@+�
@+��@+�@+33@*~�@*�@)�@)�#@)��@)�7@)hs@)%@(r�@'�;@'�w@'��@'|�@'\)@'+@&�y@&��@&v�@&ff@%�@%�@$��@$1@#��@#��@#�F@#��@#�@#t�@#t�@#t�@#dZ@"��@"^5@"M�@"�@!��@!%@ �u@ bN@ Q�@  �@�@�w@��@�P@+@��@��@ȴ@�R@�+@@��@�@O�@?}@�@��@�D@j@9X@��@33@o@�H@�H@�H@��@��@��@��@��@�\@M�@J@�^@x�@G�@��@�u@�@1'@�;@|�@l�@+@�y@v�@$�@�T@��@��@p�@p�@O�@�@�/@�@��@��@Z@�@�m@��@t�@33@o@�H@��@�!@�\@M�@J@J@�@�^@�7@X@�@��@�9@��@�u@�@1'@�;@�@�P@l�@K�@+@
=@�@��@�+@ff@V@{@�-@O�@/@�@�@�@�@�/@��@�@z�@j@9X@�@��@�m@��@C�@33@33@C�@33@
��@
��@
�\@
M�@
J@
J@
J@	��@	�#@	�^@	�^@	��@	��@	x�@	X@	&�@�`@��@�@�@r�@Q�@1'@ �@b@b@b@�@�@�w@�@��@�P@�P@|�@l�@K�@�@�@�+@ff@V@E�@E�@E�@{@�T@��@�-@��@�h@�h@�@`B@?}@�@V@V@��@�/@��@�D@I�@��@�
@�
@ƨ@�F@��@dZ@C�@"�@�H@��@��@~�@~�@n�@n�@M�@M�@-@J@�@��@�7@x�@G�@&�@%@ ��@ ��@ �@ �u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�$�A��A� �A��^A��hA���A���A�bNA�=qA��A��-A�S�A�+A�%A��A��
A��uA�ffA��yA�t�A�9XA�ĜA��A�JA��9A�I�A���A���A���A���A�=qA��A�?}A��;A��wA���A�ZA�A��uA�G�A���A��A�jA���A�z�A�M�A� �A�ffA���A���A�S�A���A��A�;dA�1'A���A�oA�x�A�E�A��#A�M�A���A���A�|�A�x�A���A�/A��/A�jA��A�n�A�%A� �A�~�A��A���A�\)A��HA�M�A�^5A�7A~��A~I�A}�A|�+A|bNA|$�A{�-A{S�A{
=Ax~�Av{At��AtffAs�TAsdZAp��Ap5?Ao|�AmS�Ak�wAj�Ai��Ah��Ah1'Ag�Af��Af$�Ad��Ac�Ab�A`~�A_O�A^ȴA^5?A]��A]x�A]�A[�
AZ(�AW%AU33ASARbAN��AN�AO/AMdZAJ�!AJ{AI`BAH�AHZAG�#AGl�AF�AFAD��ADJACXAB�/AA��AAK�AA&�A@^5A?;dA=��A<��A<JA9�PA7��A7"�A5��A4��A4^5A4(�A3l�A3;dA2ffA1A1
=A05?A/��A.�+A.$�A-�A-+A,$�A+dZA)�A)l�A)7LA)�A(��A(�A'��A'+A&z�A&  A%�^A%�A%33A$��A$bA"�A �/A $�A?}A�A{A�^A`BA�A�A��A$�A�^A
=AI�A�Ax�A�\A��A{AhsAK�A33A�A��A�/A1'A�HA �A`BAK�A;dA33AAr�A/A�wAA�AC�A
�RA	�A	��A�A��A��AQ�A�
A�yAQ�A`BA�A ��@�ƨ@��-@��P@��7@��@��/@��@�9X@��@��@���@�E�@�7L@�(�@�S�@�
=@��@�ff@�-@���@�P@�=q@�7L@��@�$�@��`@��@睲@�~�@�7@�X@�%@�9@��
@�\)@�@�Ĝ@�|�@�^5@�p�@�Q�@���@�{@��
@���@ԣ�@ԃ@���@��@�+@́@�I�@ˮ@�v�@�&�@���@� �@��;@�t�@Ɨ�@���@ċD@�dZ@��#@�r�@���@�t�@�l�@�
=@��h@�bN@�S�@��y@��`@�;d@��@��@�C�@�~�@�p�@�1@���@���@�$�@�&�@��@�(�@���@�M�@���@���@�b@�o@�V@���@���@�dZ@�"�@��\@�V@�=q@��@�J@���@�%@��@��w@�l�@�o@���@�ff@�n�@�n�@�n�@�n�@�^5@�M�@�-@�J@�@�/@��D@�ƨ@��@��R@�V@�$�@�@���@��j@�j@��
@�o@�M�@���@�V@�r�@� �@��
@��@�ȴ@�M�@��^@��j@�Z@�  @��m@��P@���@�-@��^@���@��h@�G�@��@� �@��F@��P@�t�@�l�@�\)@�K�@�K�@�33@�o@���@�v�@���@�X@��@���@���@���@��9@���@��D@�1'@��@�K�@�+@�@��@���@��@���@�O�@�r�@�I�@�1'@��;@�S�@�o@���@�^5@�E�@�=q@�5?@�$�@��@��^@���@�G�@�V@��/@���@�Z@� �@�P@+@~�y@~ȴ@~��@~5?@~5?@~5?@~$�@}�@}`B@|��@|1@{��@{dZ@{C�@{@z�@z�!@y��@y��@x�`@w�P@w�@v�+@vff@vV@vE�@vE�@v$�@uO�@s�F@sdZ@s"�@r��@rn�@q��@q��@p�9@p  @oK�@o�@n��@m�-@l�j@lI�@l�@l�@k��@k"�@i��@i7L@h�@h  @g�w@g�P@gl�@g+@f��@f�@fȴ@f��@e�T@ep�@d�/@d�@dj@dI�@dI�@d9X@d(�@c�
@ct�@co@c@b��@b�\@bM�@a&�@`��@`bN@`  @_�P@^��@^V@]�T@]�@]?}@\��@\�@\j@\I�@\9X@[�m@[S�@Z�@Z�H@Z��@Z�@Y��@Y�^@Y7L@X��@XQ�@W�@W�P@W|�@Wl�@WK�@W+@W
=@W
=@W
=@V��@V�@Vff@U�@T�/@S�
@S�@SdZ@SC�@SC�@S33@S"�@So@S@So@R�@R��@Q��@Q�@PĜ@P�@PbN@P1'@P  @O�;@O�;@O�;@O�w@O��@Ol�@OK�@O
=@N�R@Nv�@N5?@M��@MO�@L�j@Lj@L9X@L1@K�m@K�@KdZ@KS�@K"�@J��@J�\@J=q@I�@I�^@IX@I�@H��@H��@H�u@H  @G�w@G|�@GK�@G+@F�y@Fȴ@F��@FV@F5?@F{@E��@E�-@E�h@E`B@E�@D��@D�/@Dj@D1@Cƨ@C��@CS�@B��@B�!@B��@B~�@B-@A��@A�@A�#@A�^@A��@A�7@AG�@A7L@A�@@�9@@Q�@@  @@  @@  @@  @@  @@b@?�;@?
=@>ȴ@>��@>ff@>V@>5?@=�T@=`B@<z�@<(�@<1@;�
@;�@;t�@;t�@;t�@;S�@;C�@;33@;@:��@:^5@9��@9��@9X@8�9@8r�@81'@7�;@7�P@7+@7
=@6�y@6�@6�@6ff@5�T@5@5��@5�@4��@4�j@4��@4z�@4�@3��@3�@3t�@3t�@3t�@3t�@3dZ@3S�@3S�@333@2M�@1��@1��@1��@1x�@17L@0��@0�9@0�@0 �@/\)@/K�@.��@.$�@-�h@-O�@-V@,��@,z�@,j@,Z@,9X@,9X@,(�@,�@,1@+�m@+�
@+�
@+��@+�@+33@*~�@*�@)�@)�#@)��@)�7@)hs@)%@(r�@'�;@'�w@'��@'|�@'\)@'+@&�y@&��@&v�@&ff@%�@%�@$��@$1@#��@#��@#�F@#��@#�@#t�@#t�@#t�@#dZ@"��@"^5@"M�@"�@!��@!%@ �u@ bN@ Q�@  �@�@�w@��@�P@+@��@��@ȴ@�R@�+@@��@�@O�@?}@�@��@�D@j@9X@��@33@o@�H@�H@�H@��@��@��@��@��@�\@M�@J@�^@x�@G�@��@�u@�@1'@�;@|�@l�@+@�y@v�@$�@�T@��@��@p�@p�@O�@�@�/@�@��@��@Z@�@�m@��@t�@33@o@�H@��@�!@�\@M�@J@J@�@�^@�7@X@�@��@�9@��@�u@�@1'@�;@�@�P@l�@K�@+@
=@�@��@�+@ff@V@{@�-@O�@/@�@�@�@�@�/@��@�@z�@j@9X@�@��@�m@��@C�@33@33@C�@33@
��@
��@
�\@
M�@
J@
J@
J@	��@	�#@	�^@	�^@	��@	��@	x�@	X@	&�@�`@��@�@�@r�@Q�@1'@ �@b@b@b@�@�@�w@�@��@�P@�P@|�@l�@K�@�@�@�+@ff@V@E�@E�@E�@{@�T@��@�-@��@�h@�h@�@`B@?}@�@V@V@��@�/@��@�D@I�@��@�
@�
@ƨ@�F@��@dZ@C�@"�@�H@��@��@~�@~�@n�@n�@M�@M�@-@J@�@��@�7@x�@G�@&�@%@ ��@ ��@ �@ �u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�qB�}B��BĜBŢBŢBĜBĜBĜBÖBĜBŢBŢBŢBŢBŢBĜBÖB��B�qB�dB�FB�3B�B��B��B��B�PBx�BdZBR�B5?B0!B+B(�B&�B"�B�B�BoB	7BB��B�ZB�#B�
B��BƨB�!B��B�oB�+Bx�Bp�BbNBZBO�BF�BC�B=qB7LB49B0!B+B�BoBDB%B
��B
��B
�B
�#B
��B
ĜB
�qB
�LB
�-B
�B
��B
��B
�JB
�+B
�B
z�B
v�B
u�B
s�B
p�B
l�B
hsB
YB
G�B
>wB
<jB
8RB
33B
%�B
�B
�B
JB
B	��B	�B	�B	�yB	�`B	�HB	�)B	��B	��B	ÖB	�dB	�9B	�'B	�B	�B	��B	��B	��B	�uB	�%B	{�B	r�B	gmB	XB	VB	T�B	J�B	;dB	7LB	33B	1'B	.B	+B	'�B	$�B	�B	�B	�B	oB	bB	DB		7B	1B	B��B��B�B�B�`B�HB�BB�B�B�
B�#B�B�B��B��B��B��B��BŢBB�}B�qB�XB�FB�'B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�hB�PB�DB�7B�+B�B�B�B�B�B�B~�B}�Bz�Bx�Bv�Bt�Br�Bo�Bl�BjBiyBiyBiyBhsBgmBe`BbNB`BB_;B^5B^5B]/B\)BZBW
BR�BO�BM�BL�BK�BI�BG�BF�BD�BC�BB�B@�B?}B=qB;dB9XB8RB6FB5?B5?B49B49B49B33B33B2-B2-B1'B0!B0!B/B/B.B.B-B,B,B+B+B+B+B+B+B)�B)�B)�B)�B)�B)�B)�B)�B(�B(�B'�B'�B%�B%�B%�B$�B#�B$�B$�B$�B$�B'�B&�B'�B(�B'�B&�B'�B(�B-B0!B0!B.B-B.B.B0!B1'B2-B33B33B33B5?B7LB:^B;dB>wB@�BB�BD�BE�BE�BG�BJ�BJ�BJ�BM�BO�BQ�BR�BS�BW
BYB[#B\)BaHBcTBhsBk�Bl�Bm�Bo�Bo�Bp�Bp�Bp�Bq�Bs�Bv�Bx�By�By�B{�B|�B|�B|�B|�B|�B}�B}�B~�B~�B� B�B�B�+B�7B�DB�PB�PB�VB�\B��B��B��B��B��B��B��B�B�B�!B�9B�FB�XB�dB��BÖBŢBŢBǮB��B��B��B��B�B�
B�B�BB�TB�ZB�`B�`B�`B�fB�fB�fB�mB�yB�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	B	%B	1B	DB	\B	hB	�B	�B	�B	�B	 �B	"�B	%�B	(�B	+B	+B	+B	+B	.B	1'B	2-B	7LB	:^B	=qB	@�B	B�B	E�B	H�B	J�B	L�B	L�B	N�B	P�B	P�B	P�B	P�B	Q�B	T�B	YB	\)B	_;B	`BB	aHB	aHB	aHB	bNB	e`B	gmB	jB	k�B	m�B	n�B	o�B	p�B	q�B	r�B	r�B	v�B	}�B	~�B	� B	�B	�B	�B	�B	�+B	�7B	�JB	�JB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�3B	�?B	�?B	�FB	�LB	�LB	�RB	�RB	�XB	�^B	�jB	�qB	�qB	�qB	�wB	�}B	B	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�NB	�ZB	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
VB
\B
\B
bB
hB
oB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
+B
+B
,B
-B
-B
.B
.B
.B
.B
.B
/B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
:^B
:^B
:^B
;dB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��BāB��B��B�?B�SB�B�BāB�SB�B�B��B��B�?B�SBĶB�uB�BB�PB��B�TB�B�QB�B�TB�oB}BhXBWsB6FB0�B+�B)_B'�B#�B�ByB�B
�B�B�rB�zB��B�B�[B�XB��B��B�FB�7BzDBr�Bc�B[qBQBG_BD�B>�B88B4�B1[B-)BpB�B0BEB 4B
��B
�B
�/B
�<B
ŢB
��B
�8B
��B
��B
��B
��B
�6B
�B
�3B
{B
wB
vFB
tTB
q[B
m�B
kkB
[�B
I7B
?B
=<B
9�B
5�B
'B
 'B
	B
"B
�B	�<B	��B	�B	�eB	�fB	�hB	��B	յB	͟B	�mB	��B	�B	��B	��B	��B	��B	��B	�BB	��B	��B	}�B	u%B	jKB	X�B	V�B	WYB	MjB	<jB	88B	4B	1�B	.�B	+�B	(�B	&B	!B	�B	sB	[B	�B	�B		�B		lB	�B��B�B�TB�vB�RB�B�B�kB֡B׍B��BںB�7B��B��B�B��B��B�tB�aB�OB��B��B��B��B�iB�}B�cB��B�B�B��B��B�HB�;B�jB��B�	B��B�B�pB��B�#B��B��B��B�{B�uB��B��B�BB{�By�Bw�BvBtBq[Bm]Bj�Bi�Bi�Bi�Bh�Bh�BgBcTBa-B_pB^jB^�B]�B]IB[�BY1BT�BQ4BN�BM�BL~BJ�BI7BG�BEmBD�BC�BA�BA;B?�B<�B:�B9�B7�B6zB5�B4�B4�B4�B3�B3�B2|B2�B2B0�B0�B/iB/�B.}B.�B-�B-B-B,"B,qB+�B+�B+�B+�B*�B*�B*eB*KB*eB*�B*�B+B)�B)�B(�B(�B&�B'B'B&�B%,B%�B%`B&2B&�B(�B(
B(�B)�B(�B'�B(�B)*B-]B0�B1B/B-�B/ B/OB1'B1�B2aB3hB3�B4nB6+B8B;0B<�B?�BA�BC�BEmBFYBFtBH�BKBKDBK�BN�BP}BRTBS�BT�BW�BY�B[�B]Ba�Bd�BiBk�Bl�Bm�Bo�Bo�Bp�Bp�Bq'BraBtnBw2By$Bz*Bz^B|B}B}"B}B}B}"B~B~(B.BcB��B��B��B��B��B��B��B��B��B��B��B�
B�7B�jB�HB�FB�XB�QB�cB��B��B��B��B�B��B��B��B�B�KB�VB�4B�2B�MB�SB�sBڠB��B�B�B�zB�zB�zB�B�B�B�B��B�"B��B��B��B�B��B��B�	B�B�DB�qB	 OB	-B	MB	SB	tB	�B	�B	�B	B	�B	�B	�B	B	 �B	# B	&2B	)B	+6B	+B	+B	+6B	.cB	1AB	2|B	7�B	:�B	=�B	@�B	B�B	E�B	IB	J�B	MB	MB	OB	P�B	Q B	QB	QB	R:B	UgB	YKB	\]B	_VB	`\B	a|B	abB	a�B	b�B	e�B	g�B	j�B	k�B	m�B	n�B	o�B	p�B	q�B	r�B	sB	wLB	~B	B	�4B	�;B	�AB	�GB	��B	��B	��B	�dB	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�B	�&B	�>B	�0B	�B	�"B	�)B	�/B	�OB	�OB	�5B	��B	�hB	��B	�ZB	�zB	�fB	�fB	�lB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	��B	�B	�B	��B	��B	��B	� B	�B	�B	�&B	�,B	�B	�EB	�+B	�KB	�QB	�QB	�QB	�WB	�xB	�IB	�IB	�OB	�jB	�jB	�VB	�pB	�VB	�;B	�pB	�VB	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�B	�sB	�B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B
 4B
 B
;B
'B
'B
-B
-B
aB
9B
YB
?B
EB
EB
KB
fB
	RB
	lB
	RB

rB

XB
^B
^B
^B
dB
~B
�B
pB
vB
vB
}B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
!-B
!�B
"�B
"�B
$B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&B
'8B
(
B
)*B
)*B
+6B
+B
,=B
-)B
-)B
./B
./B
./B
./B
.cB
/OB
0UB
0;B
1[B
1[B
2GB
2GB
2GB
2aB
3hB
4TB
49B
49B
49B
4TB
49B
49B
4TB
4TB
4�B
5tB
6FB
6FB
6`B
6`B
7�B
7�B
7fB
8�B
8�B
:xB
:�B
:�B
;�B
<�B
=�B
=�B
>wB
>wB
>�B
?�B
?}B
?}B
?}B
?}B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
HB
J	B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
MB
NB
NB
M�B
NB
O(B
P.B
Q B
Q B
R B
RB
RB
RB
SB
S&B
SB
TB
TB
TB
TB
TFB
UB
UB
VB
VB
V9B
VB
W$B
W?B
W$B
W?B
XEB
YKB
Y1B
Z7B
ZB
ZB
Z7B
ZB
Z7B
ZB
Z7B
Z7B
Z7B
[WB
[=B
\]B
\]B
]IB
]IB
]IB
]dB
^OB
^OB
^jB
^OB
_�B
_pB
`vB
`\B
`\B
a|B
abB
abB
a|B
a|B
bhB
bNB
bhB
bhB
cnB
c�B
c�B
dtB
dtB
d�B
dtB
ezB
ezB
e�B
e�B
f�B
ffB
ezB
f�B
f�B
f�B
f�B
g�B
g�B
gmB
gmB
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
xB
w�B
w�B
x�B
x�B
x�B
y	B
y	B
x�B
zB
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{B
z�B
|B
{�B
{�B
{�B
{�B
|B
{�B
|B
}B
}"B
}B
}�B
~B
~(B
~B
~B
~(B
B
B
~�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202004290033552020042900335520200429003355202211182142482022111821424820221118214248202004300016542020043000165420200430001654  JA  ARFMdecpA19c                                                                20200418003745  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200417153828  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200417153830  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200417153831  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200417153831  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200417153831  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200417153831  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200417153831  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200417153831  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200417153831  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200417153832  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200417153832                      G�O�G�O�G�O�                JA  ARUP                                                                        20200417155341                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200417153437  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200417153412  CV  JULD            G�O�G�O�FȔ�                JM  ARSQJMQC2.0                                                                 20200421000000  CF  PSAL_ADJUSTED_QCD�9�D�9�G�O�                JM  ARCAJMQC2.0                                                                 20200428153355  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200428153355  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200429151654  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124248  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114512                      G�O�G�O�G�O�                