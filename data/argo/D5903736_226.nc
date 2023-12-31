CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-25T17:02:31Z creation      
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kd   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M\   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20180725170231  20190604094147  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�t�}��X1   @�t��G�@5D�/���dvn��O�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� DfD� D��D� D  D� D��D� DfD� D  D� D  Dy�D��D	y�D
  D
� DfD� D  D� DfD� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� DfD� D  D� DfD� D  D� D  D� D��Dy�D��Dy�D��D� D   D � D!  D!� D"fD"� D"��D#y�D$  D$� D%fD%�fD&  D&y�D'  D'� D(  D(�fD)fD)� D)��D*� D+  D+� D,fD,�fD-fD-� D-��D.y�D.��D/� D0  D0� D1  D1�fD2fD2� D2��D3y�D3��D4y�D5  D5� D6  D6� D7  D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D=  D=�fD>  D>� D?  D?y�D@  D@�fDAfDA�fDBfDB� DC  DC� DC��DD� DE  DE� DE��DF� DG  DG� DH  DHy�DI  DI�fDJ  DJy�DK  DK� DL  DLy�DM  DM�fDN  DN� DO  DO�fDPfDP�fDQ  DQ� DR  DR� DR��DSy�DS��DTy�DT��DUy�DU��DV� DWfDW�fDXfDX� DY  DY� DZ  DZ� DZ��D[y�D[��D\� D]  D]� D^  D^� D_fD_�fD`  D`y�Da  Da� Db  Db�fDc  Dcy�Dc��Dd� De  De� DffDf� Dg  Dg� DhfDh� Di  Di�fDj  Dj� Dk  Dk� DlfDl� Dm  Dm�fDnfDn� Do  Do� Dp  Dp� Dp�fDv��D�&fD��
D�3�D�w\D�>D��RD��3D�1�D��D�� D�+3D���D�qD�+�D�{�D��D��HD�Z�D�_
D��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\A�\)A�\)A�BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B��
B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C�RC��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{�RC}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C��)C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D��Dt{D�Dt{D�{Dt{D�Dt{D��Dt{D�{Dt{D�{DnD�D	nD	�{D
t{D
��Dt{D�{Dt{D��Dt{D�{Dt{D��Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dz�D�{Dt{D��Dt{D�{Dt{D��Dt{D�{Dt{D�{Dt{D�DnD�DnD�Dt{D�{D t{D �{D!t{D!��D"t{D"�D#nD#�{D$t{D$��D%z�D%�{D&nD&�{D't{D'�{D(z�D(��D)t{D)�D*t{D*�{D+t{D+��D,z�D,��D-t{D-�D.nD.�D/t{D/�{D0t{D0�{D1z�D1��D2t{D2�D3nD3�D4nD4�{D5t{D5�{D6t{D6�{D7nD7�D8nD8�D9nD9�D:nD:�D;nD;�D<nD<�{D=z�D=�{D>t{D>�{D?nD?�{D@z�D@��DAz�DA��DBt{DB�{DCt{DC�DDt{DD�{DEt{DE�DFt{DF�{DGt{DG�{DHnDH�{DIz�DI�{DJnDJ�{DKt{DK�{DLnDL�{DMz�DM�{DNt{DN�{DOz�DO��DPz�DP�{DQt{DQ�{DRt{DR�DSnDS�DTnDT�DUnDU�DVt{DV��DWz�DW��DXt{DX�{DYt{DY�{DZt{DZ�D[nD[�D\t{D\�{D]t{D]�{D^t{D^��D_z�D_�{D`nD`�{Dat{Da�{Dbz�Db�{DcnDc�Ddt{Dd�{Det{De��Dft{Df�{Dgt{Dg��Dht{Dh�{Diz�Di�{Djt{Dj�{Dkt{Dk��Dlt{Dl�{Dmz�Dm��Dnt{Dn�{Dot{Do�{Dpt{Dp��Dv�)D� �D��GD�-�D�q�D�8QD���D��pD�,)D���D��=D�%pD��D��D�%�D�u�D��QD��D�T�D�YGD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aە�AھwAڟ�Aڡ�Aڕ�A�v�A�jA�VA�C�A��AٸRAة�A�O�A�?}Aҙ�A�I�A��A���A���A�C�AÏ\A�Q�A��A��yA�;dA�{A�ffA�x�A�A�=qA���A�hsA��-A��A�A��A�ffA�bA�n�A���A��A��wA��yA��A�C�A���A�9XA�^5A��A��`A��/A�%A���A�A�O�A��/A�5?A�9XA���A��A�z�A� �A���A�ƨA�+A��uA�v�A�A�A�n�A��TA��RA��uA�A��A���A��+A�ĜA���A��HA�~�A��mA���A���A���A�&�A��9A���A�l�A�oA�^5A��A��RA�33A��A���A~�Ax�Au�wAs��At(�Aq��ApM�Ao�^An1AmVAkAi�AeO�Ab�`AaVA_��A^��A]��A\-AZM�AY�TAY�AY\)AXȴAW�7AUx�AT�AR��ARbAQ��AQ�AOƨAM�7AK
=AI�wAIAJ(�AHA�AF�+AC&�AA��A@�+A>^5A;�FA:E�A9��A7�TA5;dA4��A3;dA2E�A1hsA0�yA0ȴA0�jA0��A/�7A.��A.��A-�mA,�/A,bA+�A)�FA)`BA&��A#ƨA"�A!C�A z�A��Av�A�Az�AVA�A��A�A��A~�AS�A�DA�A�`A�A�AȴA��A��AG�A��AI�AVA�A%AQ�A�Ax�A
��A
VA	�FA	A�mA33A�AVA�AdZA(�A?}A%A��AX@���@�@�p�@���@���@���@�b@���@�%@�l�@��@�(�@�@�{@�ƨ@��@��@���@�!@�V@���@�hs@�l�@�@�=q@�@�?}@�V@ܬ@ڇ+@��@�b@�t�@�
=@��@Ցh@��@�1'@���@�o@��T@�Ĝ@�1@Ώ\@͑h@̃@�K�@ʰ!@�E�@��@��@�^5@��T@���@�1@�V@���@��h@�7L@�Ĝ@�(�@�l�@�E�@�&�@�;d@�E�@�V@�Q�@�9X@��m@�ƨ@��@�;d@�M�@��\@���@�@�ȴ@��@��@��j@�bN@�ƨ@�v�@�&�@�o@��H@�J@���@��9@��@��@���@�bN@�(�@�b@��P@�C�@�K�@�l�@��@�E�@��^@��@�@���@�A�@�/@��^@��@��@��@�`B@�X@���@�9X@�1'@�1'@�1@�t�@�K�@�o@��!@��+@�5?@�{@��@��u@�5?@�O�@��@��9@��u@�r�@�bN@��@�|�@�+@��H@��@�ȴ@��R@���@��\@�v�@�E�@���@��@�O�@�/@�/@�?}@�O�@�O�@�7L@��@��j@��@�r�@��@���@�dZ@�;d@�"�@��@��R@���@�~�@�5?@�{@��@��@�G�@�7L@�%@���@�b@���@�C�@�@�/@��@�j@��@�l�@�o@�@��@�V@�$�@��@�ff@�ff@�v�@�n�@�J@��@���@���@���@���@�X@�hs@�hs@��`@�%@�%@��9@��j@�%@��`@��j@�j@���@���@���@��!@��\@�@���@���@��@��@��T@���@��^@���@���@���@��h@�`B@�?}@��@�r�@�(�@�1@��@�C�@�5?@��\@�~�@��+@���@��\@��+@�=q@�J@���@��T@�@��7@�X@��@��`@���@�Ĝ@�r�@�A�@� �@�1@��
@���@��P@�|�@�l�@�;d@��H@���@���@���@�ȴ@���@�=q@��f@�z�@z��@s��@i�d@^8�@U��@OP�@HM@A(�@;(@5�T@-�3@&H�@�f@��@�s@�;@�?@Xy@qv1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aە�AھwAڟ�Aڡ�Aڕ�A�v�A�jA�VA�C�A��AٸRAة�A�O�A�?}Aҙ�A�I�A��A���A���A�C�AÏ\A�Q�A��A��yA�;dA�{A�ffA�x�A�A�=qA���A�hsA��-A��A�A��A�ffA�bA�n�A���A��A��wA��yA��A�C�A���A�9XA�^5A��A��`A��/A�%A���A�A�O�A��/A�5?A�9XA���A��A�z�A� �A���A�ƨA�+A��uA�v�A�A�A�n�A��TA��RA��uA�A��A���A��+A�ĜA���A��HA�~�A��mA���A���A���A�&�A��9A���A�l�A�oA�^5A��A��RA�33A��A���A~�Ax�Au�wAs��At(�Aq��ApM�Ao�^An1AmVAkAi�AeO�Ab�`AaVA_��A^��A]��A\-AZM�AY�TAY�AY\)AXȴAW�7AUx�AT�AR��ARbAQ��AQ�AOƨAM�7AK
=AI�wAIAJ(�AHA�AF�+AC&�AA��A@�+A>^5A;�FA:E�A9��A7�TA5;dA4��A3;dA2E�A1hsA0�yA0ȴA0�jA0��A/�7A.��A.��A-�mA,�/A,bA+�A)�FA)`BA&��A#ƨA"�A!C�A z�A��Av�A�Az�AVA�A��A�A��A~�AS�A�DA�A�`A�A�AȴA��A��AG�A��AI�AVA�A%AQ�A�Ax�A
��A
VA	�FA	A�mA33A�AVA�AdZA(�A?}A%A��AX@���@�@�p�@���@���@���@�b@���@�%@�l�@��@�(�@�@�{@�ƨ@��@��@���@�!@�V@���@�hs@�l�@�@�=q@�@�?}@�V@ܬ@ڇ+@��@�b@�t�@�
=@��@Ցh@��@�1'@���@�o@��T@�Ĝ@�1@Ώ\@͑h@̃@�K�@ʰ!@�E�@��@��@�^5@��T@���@�1@�V@���@��h@�7L@�Ĝ@�(�@�l�@�E�@�&�@�;d@�E�@�V@�Q�@�9X@��m@�ƨ@��@�;d@�M�@��\@���@�@�ȴ@��@��@��j@�bN@�ƨ@�v�@�&�@�o@��H@�J@���@��9@��@��@���@�bN@�(�@�b@��P@�C�@�K�@�l�@��@�E�@��^@��@�@���@�A�@�/@��^@��@��@��@�`B@�X@���@�9X@�1'@�1'@�1@�t�@�K�@�o@��!@��+@�5?@�{@��@��u@�5?@�O�@��@��9@��u@�r�@�bN@��@�|�@�+@��H@��@�ȴ@��R@���@��\@�v�@�E�@���@��@�O�@�/@�/@�?}@�O�@�O�@�7L@��@��j@��@�r�@��@���@�dZ@�;d@�"�@��@��R@���@�~�@�5?@�{@��@��@�G�@�7L@�%@���@�b@���@�C�@�@�/@��@�j@��@�l�@�o@�@��@�V@�$�@��@�ff@�ff@�v�@�n�@�J@��@���@���@���@���@�X@�hs@�hs@��`@�%@�%@��9@��j@�%@��`@��j@�j@���@���@���@��!@��\@�@���@���@��@��@��T@���@��^@���@���@���@��h@�`B@�?}@��@�r�@�(�@�1@��@�C�@�5?@��\@�~�@��+@���@��\@��+@�=q@�J@���@��T@�@��7@�X@��@��`@���@�Ĝ@�r�@�A�@� �@�1@��
@���@��P@�|�@�l�@�;d@��H@���@���@���@�ȴ@���G�O�@��f@�z�@z��@s��@i�d@^8�@U��@OP�@HM@A(�@;(@5�T@-�3@&H�@�f@��@�s@�;@�?@Xy@qv1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBB%B+BJBbBoB�B�B�B�B'�BN�B{�B�PB��BĜB�5B  BDB �BC�B[#BffBw�B{�Bz�Bz�B� B�B�=B�JB�uB��B��B��B��B�uB�{B�uB�uB�PB�PB�7B�B}�B{�Bw�BS�BJ�BW
BH�B>wB=qB=qB-B,B1'B+B�BoB
=B%B��B��B��B�B�BB��B�}B�B�%Br�Bl�B^5BZBJ�B?}B>wB;dBB�BI�BN�BT�BM�B;dB"�B�B
��B
ȴB
��B
�{B
o�B
hsB
y�B
�DB
iyB
2-B
�B
�B
'�B
�B
�B
%�B
�B
uB

=B	��B	��B	�XB	��B	��B	��B	�\B	�%B	z�B	|�B	� B	�B	�+B	�B	w�B	p�B	hsB	dZB	`BB	ZB	N�B	7LB	 �B	�B	�B	�B	VB��B�B�ZB�5B��BƨB��B�qB�LB�B��B��B��B��B��B�B�B��B�B��B��B��B��B��B��B��B�{B�bB�7B�%B�+B�B�B�B~�B}�B|�B{�By�By�Bw�Bv�Bu�Bs�Br�Bp�Bn�Bl�BjBjBiyBhsBffBe`BdZBbNBaHB`BB_;B^5B_;B]/B\)B[#B[#BYBYBYBYBZB[#BZBXBT�BQ�BI�BH�BG�BE�BB�BA�B@�B?}B@�B@�BB�B@�B?}B?}B?}B?}B?}B>wB>wB=qB<jBA�BB�BB�BC�BC�BD�BC�BC�BH�BJ�BM�BM�BM�BM�BL�BM�BO�BO�BQ�BR�BT�BT�BXBYBZB]/B]/B]/BcTBe`BgmBiyBn�Bo�Br�Bs�Bt�Bu�Bw�By�Bz�B}�B� B�B�%B�7B�=B�=B�=B�DB�=B�VB��B��B��B��B��B�B�^B�jB�jB�dB�jB�qB�jB�}BŢBŢB��B��B�B�/B�HB�ZB�fB�B�B�B�B��B��B	  B	+B	PB	\B	oB	�B	$�B	%�B	&�B	'�B	,B	,B	.B	2-B	2-B	2-B	33B	8RB	:^B	;dB	=qB	=qB	=qB	<jB	;dB	<jB	:^B	;dB	<jB	>wB	?}B	?}B	@�B	D�B	F�B	H�B	K�B	N�B	O�B	O�B	Q�B	Q�B	Q�B	S�B	VB	\)B	`BB	cTB	dZB	e`B	ffB	gmB	jB	p�B	s�B	s�B	v�B	x�B	{�B	� B	�B	�B	�%B	�+B	�+B	�+B	�DB	�JB	�\B	�oB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�RB	�wB	B	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�/B	�;B	�NB	�ZB	�ZB	�ZB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
{B

	B
OB
)_B
0oB
8�B
88B
AoB
I�B
Q4B
T{B
Z�B
^�B
d@B
i�B
o�B
t�B
w�B
z^B
}B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�B
�%B
�-B
�MB
�bB sB�B	�B	�B�B�B<�Bi�B{EB��B��B�#B��B�.B�B1sBI BTABe�Bi�Bh�Bh�Bm�Br�BxBz&B�IB�[B�]B�`B�WB�MB�SB�OB�QB{.B{-BwBo�Bk�Bi�Be�BA�B8�BD�B6�B,dB+YB+YB�B�BB�B
�B bB�,B�B��B��B�BۉB�@B��B�B� Bt-B`�BZ�BLEBH/B8�B-�B,�B)zB0�B7�B<�BCB;�B)zB�B�B
��B
��B
�B
��B
]�B
V�B
hB
yyB
W�B
 lB
�B
�B
6B
�B
�B
'B

�B
�B	��B	� B	�DB	��B	�MB	�
B	��B	}�B	t�B	iCB	kMB	nbB	pkB	u�B	qoB	f-B	_B	V�B	R�B	N�B	H�B	==B	%�B	4B	�B	B	B��B�JB�	B��B̮B�wB�(B�B��B��B��B�aB�VB�MB�<B�]B��B��B�B��B�wB�bB�XB�MB�=B�3B�'B�
B~�Bw�Bt�Bu�Bs�Br�Bo�Bm�Bl�Bk~BjzBhnBhmBfbBe`BdYBbGBaDB_8B]-B[ BYBYBXBWBT�BS�BR�BP�BO�BN�BM�BL�BM�BK�BJ�BI�BI�BG�BG�BG�BG�BH�BI�BH�BF�BC�B@�B8[B7RB6NB4@B10B0&B/#B.B/&B/$B11B/&B.B. B.B.B.!B-B-B,B+B0.B1:B16B2:B2<B3?B2;B2;B7UB9gB<vB<vB<yB<vB;nB<wB>�B>�B@�BA�BC�BC�BF�BG�BH�BK�BK�BK�BQ�BTBVBXB]<B^@BaSBbZBcaBddBfoBh�Bi�Bl�Bn�Bs�Bt�Bw�Bx�Bx�Bx�By�Bx�B|�B�0B�]B�rB��B��B��B��B�B�B��B�B�B�B�B�<B�?B�lB��BėB��B��B��B��B� B�>B�BB�LB�XB�sB�B��B��B��B	 �B	@B	kB	pB	vB	{B	�B	�B	�B	 �B	 �B	 �B	!�B	&�B	(�B	)�B	+�B	+�B	+�B	*�B	)�B	*�B	(�B	)�B	*�B	-B	.B	.	B	/B	3)B	51B	7=B	:QB	=dB	>eB	>hB	@sB	@sB	@vB	B~B	D�B	J�B	N�B	Q�B	R�B	S�B	T�B	U�B	YB	_*B	b=B	b=B	ePB	g[B	jmB	n�B	p�B	q�B	t�B	u�B	u�B	u�B	y�B	z�B	}�B	��B	��B	��B	��B	��B	�B	�B	�B	�8B	�LB	�^B	�dB	�gB	�vB	�|B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�.B	�*B	�*B	�(B	�+B	�'B	�3B	�XB	�fB	�wB	āB	�}B	ńB	ǐB	ȗB	ǒB	ǏB	ȖB	˨B	ͰB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�'B	�+B	�3B	�?B	�JB	�NB	�OB	�YB	�XB	�[B	�dB	�eB	�eB	�iB	�iB	�pB	�oB	�vB	�sB	�~B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�G�O�B	�~B
�B
�B
�B
'+B
&�B
/�B
8DB
?�B
B�B
H�B
L�B
R�B
X6B
^ZB
cHB
e�B
h�B
kvB
o�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.18 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9995(+/-0.0001), vertically averaged dS =-0.017(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941472019060409414720190604094147  AO  ARCAADJP                                                                    20180725170231    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180725170231  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180725170231  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094147  IP                  G�O�G�O�G�O�                