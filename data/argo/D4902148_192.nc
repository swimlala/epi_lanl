CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-19T15:38:28Z creation;2019-11-19T15:38:32Z conversion to V3.1;2022-11-21T05:28:01Z update;     
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
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    B    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  D    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  M�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    U�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    _�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    i�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  k�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    s�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  u�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20191119153828  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_192                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @��d� 1   @����Ԁ@;Ʌ�oiD�dk��$�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(y�D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D�fD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(y�D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D�fD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��#A��#A��HA��A��A��A���A���A���A���A��A��HA×�A��HA�7LA�9XA�S�A�G�A�{A��yA�^5A�~�A�=qA�1A��
A�ĜA�p�A���A�M�A��A�"�A�33A���A��!A�A�A�ȴA�t�A��A��^A�  A���A�
=A�5?A�bA���A�|�A��A��A�G�A�1A��A�VA��uA�+A��A��uA�A���A�Q�A��!A�XA�~�A��9A���A�~�A�Q�A�A�A�5?A�-A�oA�|�A�r�A�A��\A�A�-A�XA���A��A���A�{A���A��A�E�A��A~ĜA~1'A}VAzJAxbNAwK�Av�!Av��Avr�Av�AudZAs��Ar�9Aq�wAqhsApz�Am�mAl�Akt�AkS�Ak
=Aj��Ai�
Ae�AeVAdbNAc�hAc7LAcoAb��AbI�Aa�AaoA_��A^�/A^�jA\(�AXȴAW�AV�AV�jAV^5AVbAU7LAS|�ARr�AR{AQ�;AQp�AO�mAN��AM�AL5?AKl�AJ�uAI�AI�7AH��AG�AF^5AD�!AD$�AB�yAAl�A@��A?��A>��A;�A:�uA:5?A9�TA9�A8~�A8I�A8$�A8JA7��A7�#A7t�A6��A5�mA5K�A4r�A3��A2�A2  A1�
A1��A0��A0E�A.�\A-�
A-�A,�+A+��A+dZA*�jA*(�A(��A(1A&��A&5?A$ȴA#?}A"��A"v�A"bA �A��A�AbNAA��A
=AA��A�wA�yA�DA-A��A�7A%AZAdZA�A��A�AE�AĜA�hA33A�#A
-A	7LA��A�yA�/AbA��A(�A��AdZA/A�AE�A�A�yA{A 5?@�;d@�E�@��/@�33@�{@��@�J@�K�@�@�{@�-@�?}@�E�@�9@�r�@�1@��@�ȴ@�h@��m@���@�^@�hs@�V@䛦@�(�@�F@���@���@�`B@߅@�ȴ@�E�@��/@���@��@֏\@�{@���@թ�@Ցh@�p�@�G�@��@�Ĝ@�M�@�C�@��@�p�@��`@�Q�@�\)@�=q@ɩ�@�X@ȃ@�K�@Ƈ+@őh@ě�@�|�@��@��@�$�@��-@���@�A�@���@�|�@�S�@�"�@�
=@��H@��@��@�r�@�$�@��/@��9@��@��u@��
@�@��\@�@�x�@��P@�?}@�Ĝ@���@�A�@��;@�@�ff@�J@���@�X@�/@���@�1@���@�\)@�33@�
=@��H@�ȴ@�5?@���@�Ĝ@���@�|�@�+@��@���@�ff@�hs@���@�j@��w@���@�{@��#@�p�@�X@�V@���@�I�@�dZ@��H@���@�^5@��^@�?}@�%@��@�9X@��;@�ƨ@�"�@�=q@�-@�@�X@�bN@�A�@�b@��@��@�~�@�E�@���@��T@��-@�%@���@��@�bN@�b@�C�@���@��+@��@���@��h@��7@�x�@�x�@��@��9@��u@�z�@�j@�A�@���@�@�n�@�5?@�@�@��h@�x�@�X@�7L@��@�Ĝ@��u@�z�@�Z@���@�;d@�"�@��@�ȴ@��+@��+@�=q@��^@���@�x�@�`B@�7L@��/@��j@��9@��@�bN@�I�@�1'@� �@�1@��@\)@�@~@|�@|(�@{�F@{S�@z�\@y��@yx�@yX@y&�@x��@xr�@w��@v�R@u�T@u��@u��@u@u�-@u�h@uO�@u�@t�@t��@t��@t�D@tz�@tZ@t(�@s��@sS�@q�#@p��@p��@p��@p�9@p1'@pb@o�;@o�w@o�P@o|�@o+@n�@n�+@n{@n@m�-@m/@m�@l��@l�/@l�j@lI�@k��@k"�@k@j�@j�H@j��@j�!@j�@k1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��#A��#A��HA��A��A��A���A���A���A���A��A��HA×�A��HA�7LA�9XA�S�A�G�A�{A��yA�^5A�~�A�=qA�1A��
A�ĜA�p�A���A�M�A��A�"�A�33A���A��!A�A�A�ȴA�t�A��A��^A�  A���A�
=A�5?A�bA���A�|�A��A��A�G�A�1A��A�VA��uA�+A��A��uA�A���A�Q�A��!A�XA�~�A��9A���A�~�A�Q�A�A�A�5?A�-A�oA�|�A�r�A�A��\A�A�-A�XA���A��A���A�{A���A��A�E�A��A~ĜA~1'A}VAzJAxbNAwK�Av�!Av��Avr�Av�AudZAs��Ar�9Aq�wAqhsApz�Am�mAl�Akt�AkS�Ak
=Aj��Ai�
Ae�AeVAdbNAc�hAc7LAcoAb��AbI�Aa�AaoA_��A^�/A^�jA\(�AXȴAW�AV�AV�jAV^5AVbAU7LAS|�ARr�AR{AQ�;AQp�AO�mAN��AM�AL5?AKl�AJ�uAI�AI�7AH��AG�AF^5AD�!AD$�AB�yAAl�A@��A?��A>��A;�A:�uA:5?A9�TA9�A8~�A8I�A8$�A8JA7��A7�#A7t�A6��A5�mA5K�A4r�A3��A2�A2  A1�
A1��A0��A0E�A.�\A-�
A-�A,�+A+��A+dZA*�jA*(�A(��A(1A&��A&5?A$ȴA#?}A"��A"v�A"bA �A��A�AbNAA��A
=AA��A�wA�yA�DA-A��A�7A%AZAdZA�A��A�AE�AĜA�hA33A�#A
-A	7LA��A�yA�/AbA��A(�A��AdZA/A�AE�A�A�yA{A 5?@�;d@�E�@��/@�33@�{@��@�J@�K�@�@�{@�-@�?}@�E�@�9@�r�@�1@��@�ȴ@�h@��m@���@�^@�hs@�V@䛦@�(�@�F@���@���@�`B@߅@�ȴ@�E�@��/@���@��@֏\@�{@���@թ�@Ցh@�p�@�G�@��@�Ĝ@�M�@�C�@��@�p�@��`@�Q�@�\)@�=q@ɩ�@�X@ȃ@�K�@Ƈ+@őh@ě�@�|�@��@��@�$�@��-@���@�A�@���@�|�@�S�@�"�@�
=@��H@��@��@�r�@�$�@��/@��9@��@��u@��
@�@��\@�@�x�@��P@�?}@�Ĝ@���@�A�@��;@�@�ff@�J@���@�X@�/@���@�1@���@�\)@�33@�
=@��H@�ȴ@�5?@���@�Ĝ@���@�|�@�+@��@���@�ff@�hs@���@�j@��w@���@�{@��#@�p�@�X@�V@���@�I�@�dZ@��H@���@�^5@��^@�?}@�%@��@�9X@��;@�ƨ@�"�@�=q@�-@�@�X@�bN@�A�@�b@��@��@�~�@�E�@���@��T@��-@�%@���@��@�bN@�b@�C�@���@��+@��@���@��h@��7@�x�@�x�@��@��9@��u@�z�@�j@�A�@���@�@�n�@�5?@�@�@��h@�x�@�X@�7L@��@�Ĝ@��u@�z�@�Z@���@�;d@�"�@��@�ȴ@��+@��+@�=q@��^@���@�x�@�`B@�7L@��/@��j@��9@��@�bN@�I�@�1'@� �@�1@��@\)@�@~@|�@|(�@{�F@{S�@z�\@y��@yx�@yX@y&�@x��@xr�@w��@v�R@u�T@u��@u��@u@u�-@u�h@uO�@u�@t�@t��@t��@t�D@tz�@tZ@t(�@s��@sS�@q�#@p��@p��@p��@p�9@p1'@pb@o�;@o�w@o�P@o|�@o+@n�@n�+@n{@n@m�-@m/@m�@l��@l�/@l�j@lI�@k��@k"�@k@j�@j�H@j��@j�!@j�@k1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B&�B&�B!�B#�BoB��B��B�B�B�B�B�B�/B�BƨB�RB�!B��B��B�oB�Br�Bn�BjBaHB[#BW
BVBS�BQ�BM�BG�B=qB2-B$�B�B�BuBPBB��B�B�B��B�VB�PB�DB�B|�By�Bw�Bv�Bt�Br�Bn�BdZBK�B%�B  B
�5B
��B
��B
�}B
�B
��B
�hB
�DB
�+B
� B
y�B
t�B
l�B
ZB
N�B
G�B
C�B
B�B
A�B
>wB
9XB
/B
&�B
 �B
�B
�B
+B	��B	��B	��B	��B	�B	�yB	�B	��B	��B	ɺB	ǮB	ƨB	ŢB	B	��B	�jB	�?B	�!B	�B	��B	�PB	�+B	�B	�B	�B	� B	{�B	s�B	n�B	m�B	k�B	iyB	dZB	`BB	ZB	Q�B	M�B	I�B	F�B	D�B	@�B	<jB	6FB	.B	+B	$�B	�B	�B	{B	JB	B��B��B��B�B�B�B�B�B�B�B�B�fB�HB�;B�;B�)B�B�
B�
B��B��B��BǮBÖB�}B�jB�^B�LB�3B�B��B��B��B��B��B��B��B��B�uB�\B�DB�1B�B�B|�Bx�Bt�Bp�Bm�Bk�BjBiyBgmBffBe`BbNB`BB^5B\)BYBVBR�BP�BN�BJ�BG�BE�BE�BD�BC�BA�B?}B>wB=qB<jB;dB:^B9XB8RB6FB49B33B1'B0!B/B-B,B)�B'�B'�B&�B&�B&�B$�B$�B$�B$�B$�B#�B"�B!�B!�B �B �B �B �B �B�B�B�B�B�B�B�B�B�B�B�B �B �B �B!�B!�B!�B!�B �B�B!�B"�B"�B"�B#�B"�B#�B$�B$�B$�B%�B&�B'�B(�B(�B(�B+B,B+B+B+B.B0!B0!B0!B0!B0!B0!B2-B:^BA�BG�BK�BK�BK�BK�BM�BO�BO�BP�BQ�BVB]/B^5B_;B`BB`BBcTBe`BgmBgmBiyBiyBjBm�Bn�Bo�Bo�Bo�Bo�Bo�Bq�Br�Bu�By�Bz�B{�B|�B|�B|�B�B�B�B�1B�DB�PB�bB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�?B�?B�FB�RB�jB�}B��B��BBÖBƨBȴBȴBɺB��B��B��B�B�
B�B�B�)B�/B�5B�HB�TB�TB�ZB�ZB�`B�`B�B�B�B�B��B��B��B��B��B��B��B	B	B	B		7B	VB	\B	hB	oB	�B	�B	�B	�B	�B	 �B	!�B	"�B	%�B	%�B	&�B	'�B	(�B	)�B	+B	+B	,B	-B	.B	.B	2-B	7LB	9XB	;dB	<jB	@�B	B�B	D�B	E�B	F�B	G�B	H�B	K�B	N�B	P�B	Q�B	Q�B	Q�B	Q�B	R�B	S�B	VB	W
B	XB	YB	ZB	ZB	ZB	[#B	\)B	^5B	bNB	dZB	e`B	e`B	e`B	gmB	iyB	jB	k�B	l�B	n�B	o�B	o�B	p�B	r�B	r�B	s�B	t�B	t�B	u�B	v�B	v�B	x�B	|�B	~�B	� B	�B	�B	�B	�1B	�DB	�J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�BsBsB�B�B�B�B�B�B�B�ByB�B�B�B�B'B'mB"�B%,B�B��B�8B�B�!B�B�B� B�B�xB�~B�PB��B�0B�@B�#B�Bs�Bp�Bm�BdZB\�BW�BV�BT�BSuBO�BI�B@�B5tB'RB;BBFBVB�B�fB�KB�AB��B�bB�B��B�B}<BzBxBwButBt�Bq�Bh�BQ4B+�B�B
��B
��B
͹B
��B
�[B
�B
��B
�B
�KB
�B
z�B
v�B
o�B
\B
PB
HKB
C�B
B�B
B'B
?�B
:�B
0�B
(
B
!�B
VB
QB
�B
 4B	�>B	�lB	��B	�B	�CB	�
B	��B	͹B	�#B	��B	�+B	�?B	�GB	��B	��B	�zB	�[B	�5B	�B	��B	��B	��B	��B	��B	�UB	}�B	t�B	o5B	nB	l�B	k6B	e�B	a�B	\B	SB	N�B	J�B	GzB	E�B	BB	>B	8B	/B	,�B	&�B	�B	�B	�B	(B	�B��B�B��B�nB��B��B��B��B�B�OB�B�mB�NB�\B�BBݘBٴB�sB׍B�B�@B��BȴBĜB�OB�<B�JB�RB�TB��B�QB�sB�B��B�pB�kB�B�sB�B��B�JB�RB�tB�3B~]Bz^BvzBq�Bn�Bl"BkBjBh
BgRBffBc�Ba-B^�B^BZ�BW�BT{BQ�BP�BL�BH�BFBE�BEBD�BB�B@�B?B>B<�B<B;JB:^B9�B7�B6`B4B1�B1AB0UB./B-�B+�B)yB(sB'mB'mB'�B&�B%�B%,B%FB%`B$�B#�B"�B"�B!|B!B!-B!-B!HB BB vB �BVB�B \BpBB�BB�B!-B!B!B!�B!�B"B"4B!�B!�B#�B#�B#TB#TB$tB#�B$�B%`B%FB%�B&�B'�B(�B)�B)�B*KB,WB-B+QB+kB+�B.}B0UB0UB0UB0oB0�B1AB4TB<BB�BHfBLBK�BL0BLdBNVBPHBP}BQ�BS[BWYB]~B^�B_�B`�B`�Bc�Be�Bg�Bg�Bi�Bi�BkBm�Bn�Bo�Bo�Bo�Bo�Bp!Br-BshBv`Bz*B{0B|6B}<B}VB}�B�oB��B��B��B��B��B��B��B��B��B��B�$B��B��B��B�#B�B�B�B�4B�,B�2B�mB��B�IB�}B��B��B�tB��B��B��B��B��B��B��B��B��B��B��B��B�#B�JB�4B�2B�SB�YB�1B�7B�CB�IBބB�B�B�nB�tB�B��B�B��B��B��B��B��B��B�B�	B��B�0B�B	 B	'B	mB		�B	pB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	# B	%�B	%�B	'B	(
B	)B	*0B	+B	+6B	,=B	-CB	.IB	.}B	2�B	7fB	9�B	;�B	<�B	@�B	B�B	D�B	E�B	F�B	G�B	IB	LB	O(B	Q B	Q�B	RB	RB	RB	SB	S�B	V9B	W$B	X+B	Y1B	ZB	Z7B	Z7B	[WB	\xB	^�B	b�B	dZB	ezB	e�B	e�B	g�B	i�B	j�B	k�B	l�B	n�B	o�B	o�B	p�B	r�B	r�B	s�B	t�B	t�B	u�B	v�B	v�B	y$B	}"B	B	�B	� B	�-B	�B	�1B	�DB	�J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912041146482019120411464820191204114648202211182141002022111821410020221118214100201912060029212019120600292120191206002921  JA  ARFMdecpA19c                                                                20191120003719  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191119153828  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191119153830  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191119153831  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191119153831  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191119153831  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191119153831  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20191119153831  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20191119153831  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191119153831  QCF$                G�O�G�O�G�O�            8000JA      jafc1.0                                                                 20191119153832                      G�O�G�O�G�O�                JA  ARUP                                                                        20191119155333                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20191119153206  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20191119153153  CV  JULD            G�O�G�O�F�h�                JM  ARCAJMQC2.0                                                                 20191204024648  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191204024648  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191205152921  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124100  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                