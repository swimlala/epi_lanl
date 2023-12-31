CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-02-17T09:57:15Z creation; 2023-04-26T19:14:29Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dt   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200217095715  20230426191429  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               M   MAA  AOAO7316_008644_077                 7316_008644_077                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @��ǣ� @��ǣ� 11  @��
�L0@��
�L0@)txB0��@)txB0���d���L�d���L11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?��H@B�\@�  @�G�@��
@�\A   A  A ��A,��A@��A`��A�Q�A�  A��A�  A�  A�  A߮A�A��B  B  B  B�B'�B0  B8  B@  BH  BP(�BX  B`  Bh  Bp(�Bx(�B�  B�  B�  B�  B�  B�{B�(�B�Q�B��B�{B�{B��B�{B�  B��B�  B�{B�  B�  B�  B�  B�  B��B�  B�  B��B�  B��B��B��B��
B��B��C  C
=C  C��C	��C
=C{C
=C
=C
=C{C
=C  C��C��C   C"  C#��C%�C'��C)��C,  C.  C/��C1��C4
=C6  C8  C:  C;��C>  C@  CA��CC��CE��CH
=CJ  CL  CN  CP  CR
=CT  CV{CX  CY��C\  C^  C`  Cb  Cd
=Cf
=Ch  Ci��Ck��Cn  Cp
=Cr  Ct  Cv
=Cx  Cy��C{��C~  C�C�  C���C�  C���C���C�  C���C���C�  C�  C�  C�  C�C�
=C�C�  C���C�  C�  C���C���C�  C�C�  C�C�
=C�  C�C�C���C�  C�  C���C�  C�C�C�C�C�  C�C�  C�  C�C�  C���C�  C���C���C���C���C�  C�  C���C���C�C�C�
=C�
=C���C���C���C���C���C�
=C�C�  C�C�  C�  C�  C�  C�  C�  C���C�  C�  C���C�  C�  C�  C���C���C�  C�C�C�C�
=C�  C�  C�C�
=C�
=C�C�  C�  C�  C�  C�C���C���C���C���C�  C�C���C���C���C�  C�C�C�  C�  C���C���C�  C�  C�  C�  C�C�C���C�  C�  C���C�  C�  C�D �D � D ��D� D�D��D  D� D�qD}qD��D� DD��D  D� D  D}qD��D	z�D	�qD
�DD� D  D�D�D��D  D� D�D�D  D}qD  D��D  D}qD�qD� D�D}qD�qDz�D  D�D�D��D  D}qD�D�D�D}qD  D�D�D}qD  D� D  D�D�D� D   D � D!  D!� D"  D"}qD"�qD#� D$�D$� D$�qD%� D&  D&}qD&�qD'}qD(  D(� D)  D)}qD)�qD*� D*�qD+}qD+�qD,� D-  D-� D.�D.� D.�qD/}qD/�qD0}qD1  D1��D2  D2� D3  D3}qD4  D4}qD4�qD5}qD6  D6� D7�D7� D7�qD8� D9  D9� D:  D:}qD;  D;� D;�qD<}qD<�qD=}qD>  D>� D?  D?��D@�D@� DA  DA}qDB  DB}qDB�qDC� DD  DD� DE�DE��DF  DF� DF�qDGz�DG�qDH� DI  DI}qDI�qDJ� DJ�qDK� DLDL��DM�DM� DM�qDN� DO  DO� DO�qDP� DQ�DQ� DR  DR� DS  DS� DT�DT�DU  DU� DVDV� DV�qDW� DX  DX� DY�DY� DY�qDZ}qD[  D[}qD[��D\}qD]  D]� D^  D^� D_  D_}qD`  D`��Da�Da� Da�qDb}qDc  Dc}qDc�qDd}qDd�qDe� De�qDf}qDg  Dg}qDg�qDh}qDh�qDi}qDi�qDj��Dk�Dk}qDl  Dl� Dm  Dm� Dn  Dn��Do  Do}qDo��Dp��Dq�Dq� Dr  Dr��DsDs��Dt  Dt� Du  Du� Dv�Dv� Dv�qDw}qDx  Dx��Dy  Dy}qDz  Dz}qDz��D{}qD|  D|}qD|�qD}� D~�D~� D�D� D�qD�AHD�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD���D�� D���D�=qD�}qD�� D�HD�@ D�}qD���D�  D�>�D�~�D��HD�HD�@ D�~�D���D�  D�AHD�� D���D���D�AHD���D��HD�  D�AHD���D�D�HD�AHD��HD��HD�HD�AHD�� D�� D�HD�AHD���D��HD�HD�B�D���D�� D���D�@ D�~�D��qD��qD�=qD�� D��HD�  D�@ D��HD��HD�  D�>�D�~�D��HD�  D�>�D�~�D���D���D�>�D�� D�� D�HD�AHD�� D�� D���D�>�D�� D�� D�HD�@ D��HD�� D���D�>�D�}qD�� D��D�@ D�� D���D�  D�@ D��HD���D�  D�AHD�� D��HD�HD�AHD�� D���D�  D�@ D�� D��HD�  D�AHD�� D�D��D�>�D�~�D�� D���D�@ D�~�D��HD�  D�@ D��HD���D�HD�@ D�� D�� D���D�>�D�� D��HD�HD�AHD�� D���D���D�=qD�� D���D���D�>�D�~�D���D�  D�>�D�~�D��qD��qD�@ D��HD���D���D�@ D�~�D�� D�HD�B�D��HD�� D�HD�AHD��HD�� D�  D�@ D�� D��qD�  D�AHD�~�D�� D���D�>�D�~�D���D�  D�@ D��HD�� D�HD�B�D�}qD���D���D�>�D�}qD���D���D�@ D�� D�� D��D�@ D�� D��HD�HD�>�D�~�D���D�  D�B�D�� D��qD��qD�@ D��HD�� D�HD�B�D��HD�� D���D�@ D���D�� D�  D�>�D�� D�� D���D�>�D�� D���D�  D�AHD�� D��qD��qD�@ D��HD�� D���D�@ D��HD��HD�  D�>�D�� D���D���D�>�D��HD��HD�  D�@ D�� D�� D�  D�@ D�~�D¾�D���D�@ DÀ D��HD�  D�@ DĀ D�� D�HD�AHDŁHD�� D���D�>�DƁHD��HD�HD�AHD�~�DǾ�D�  D�AHD�~�D�� D�  D�>�D�~�D�� D�  D�@ Dʀ D��HD�  D�>�Dˀ D�� D�  D�AHD̀ D�� D�  D�@ D�~�D�� D�  D�>�D�~�D�� D���D�@ Dπ D�� D�  D�AHD�~�D�� D�  D�AHDт�D�� D�  D�>�DҀ DҾ�D���D�@ DӀ D��HD�  D�AHDԀ D�� D�  D�@ DՀ D�� D�  D�@ D�~�D־�D���D�>�D�~�D�� D�  D�@ D؁HD�D�  D�>�DفHD��HD�  D�@ D�~�Dھ�D���D�@ Dۀ D�� D�HD�@ D܁HD�� D�  D�@ D�~�D�� D�HD�AHDށHD�D�  D�@ D߁HD��HD�  D�>�D��HD��HD���D�>�D� D�� D�HD�>�D�}qD⾸D�  D�>�D�~�D㾸D���D�>�D�~�D�� D���D�@ D� D�� D�  D�@ D�~�D�qD���D�>�D� D��HD�HD�AHD� D辸D���D�@ D�HD��HD�  D�AHD� D꾸D���D�@ D�~�D뾸D�  D�>�D�~�D�� D��qD�>�D� D�� D�HD�>�D�HD��HD�  D�@ D� D��HD�HD�AHD���D��HD���D�@ D�HD�D���D�AHD�HD�D�HD�@ D�~�D��HD��D�AHD�~�D��HD�HD�@ D�� D���D�  D�AHD�� D���D�  D�AHD�� D���D�  D�AHD�� D�� D��D�AHD�� D�� D�HD�@ D�}qD���?�?8Q�?u?���?\?�@�\@��@333@B�\@O\)@fff@z�H@��@��@�p�@�ff@�{@�(�@�ff@�\)@�p�@�@��@�(�Az�A��A{Az�A=qA�RA$z�A*=qA0  A4z�A9��A?\)ADz�AH��AN{ATz�AY��A]p�Ab�\AhQ�Amp�Aq�AuA{�A���A��HA���A��A�=qA�z�A��RA�G�A��
A�ffA�Q�A��\A��A�  A�=qA�z�A�
=A��A�(�A�ffA�G�A�(�A��RA���A��HA�A�  A�=qA��A�\)A�G�A˅A�ffAУ�A��HA���A�\)A��A�(�A�{A�  A��HA��A�\)A���A��
A�{A�  A�A�(�A�ffA�  A���A��
A�A��B ��BB�HB�Bz�BB�HB�
B��B	��B
�RB�
B��B��B�RB�B��B��B�\B�
B��B�B�HB�
B�B{B
=B(�BG�B=qB33B (�B!p�B"ffB#\)B$Q�B%��B&�\B'�B(��B*{B+33B,Q�B-p�B.�\B/�
B0��B1�B333B4z�B5G�B6�\B7�
B8��B9�B;
=B<Q�B=p�B>=qB?\)B@��BABB�RBC�BD��BE�BF�HBG�
BH��BI�BJ�HBK�BL��BM�BN�HBO�
BP��BQ�BS
=BS�
BT��BU�BW33BX  BX��BY�B[
=B\(�B\��B]�B^�RB_�
B`��BaBb�\Bc\)BdQ�BeG�Bf{Bf�HBg�
Bh��Bj{Bj�HBk�
Bm�BnffBo�Bp��Bq��Br�RBs�
Bu�Bv=qBw33Bx(�Byp�Bz�RB{�
B|��B}�B
=B�(�B��RB�33B�B�ffB�
=B���B�{B��RB�\)B�  B��\B�
=B��B�=qB��HB���B�(�B��RB�G�B�  B���B�33B�B�Q�B�
=B���B�=qB��RB�G�B��B��\B�33B�B�Q�B��HB��B�(�B��HB�p�B��B�z�B�
=B��B�  B�ffB���B��B�G�B��B�B�  B�{B�(�B�Q�B�z�B���B���B��HB���B��B�\)B��B��B��B�{B�=qB�Q�B���B���B��B�\)B��B�B�  B�=qB��\B���B��B�G�B��B�B�  B�Q�B���B��HB���B�33B�p�B�B�{B�ffB���B���B���B�G�B��B�B�{B�Q�B�z�B���B��HB�
=B�G�B��B��
B�  B�(�B�Q�B�z�B���B���B���B�33B�\)B���B��
B�  B�(�B�Q�B�ffB���B��HB��B�\)B��B��B�(�B�Q�B��\B���B�
=B�G�B���B��B�(�B�ffB���B���B�
=B�G�B��B��B�  B�=qB�z�B��RB���B�33B�\)B��B��
B�{B�=qB�z�B���B�
=B�\)B���B��
B�  B�Q�B�z�B���B���B��B�\)B���B��B�=qB�z�B���B�
=B�G�B��B��
B�  B�Q�B���B��HB�33B��B��
B�(�B��\B��HB�33B��B��
B�=qB��\B��HB�G�B���B��B�=qB��\B���B�33B��B��B�=qB��\B��HB�33B��B��B�(�B\B��HB�33BÙ�B��B�Q�Bģ�B�
=B�\)BŮB�{B�ffBƣ�B�
=B�\)BǮB�  B�Q�Bȣ�B���B�G�BɅB��
B�(�B�z�B���B�
=B�\)BˮB�  B�Q�B̏\B��HB�33B�p�B�B�{B�Q�BΏ\B���B�
=B�\)Bϙ�B��
B�(�B�ffBУ�B���B�33B�p�B�B�  B�=qB�z�B���B��B�\)BӮB�  B�Q�Bԣ�B���B�\)Bՙ�B�  B�Q�B֣�B���B�\)B׮B�{B�ffB���B�33BمB��B�Q�BڸRB��BۅB��B�ffBܸRB�33Bݙ�B�  B�z�B��HB�33Bߙ�B�  B�z�B��HB�G�B�B�(�B�\B�
=B�B��B�ffB��HB�\)B�B�(�B��B��B癚B�  B�z�B���B�p�B��
B�Q�B�RB�33B�B�(�B��B��B��B�{B�\B�
=B�B��B�ffB���B�G�B�B�=qB�RB�33B�B�(�B���B��B���B�  B�z�B���B�p�B��
B�Q�B��RB�33B�B�=qB��RB�33B�B�Q�B���B�\)B��
B�Q�B��RB�G�B��C �C ffC �C �C33Cz�C�RC��C33Cp�C�C�C33Cz�C�RC  C=qCz�C�RC  C33Cp�C�C�C(�CffC��C�
C{CQ�C�CC  C=qCz�C�RC	  C	=qC	z�C	�RC
  C
33C
p�C
�C
�C(�C\)C��C�HC�CffC�C�C(�CffC��C�HC�C\)C��C�HC(�C\)C��C��C{CQ�C��C�HC�CQ�C�\C��C{C\)C��C�HC�CQ�C�\C��C{CQ�C�\CC��C=qC�CC  CG�C�C�RC��C33Cz�CC  C=qCp�C�RC  C=qCz�C�RC  CG�C�C��C
=C=qC�\C�HC�C\)C��C�C33C�C��C
=CG�C�\C�C33Cz�CC 
=C Q�C ��C ��C!33C!z�C!�RC"
=C"\)C"�C"��C#=qC#�C#��C${C$ffC$�RC%  C%=qC%�C%�
C&(�C&z�C&C'
=C'Q�C'��C'��C(Q�C(�\C(�
C)(�C)�C)�
C*�C*ffC*�RC+
=C+ffC+�C+��C,G�C,��C,��C-33C-�C-��C.33C.z�C.C/{C/ffC/C0
=C0\)C0��C1
=C1Q�C1��C1��C2Q�C2�C2�C3G�C3��C4  C4G�C4��C4��C5Q�C5��C5�HC6=qC6��C6�C7=qC7�C7�C8G�C8�\C8�HC9=qC9��C9�HC:33C:�\C:�C;G�C;�\C;�HC<33C<�\C<�C==qC=�\C=�C>G�C>��C>�C?33C?�\C?�C@G�C@�\C@�HCA=qCA��CA�HCB33CB�CB�HCC=qCC�CC�
CD33CD�\CD�CE33CE�CE�HCF33CF�CF�
CG33CG�\CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                   ?u?��H@B�\@�  @�G�@��
@�\A   A  A ��A,��A@��A`��A�Q�A�  A��A�  A�  A�  A߮A�A��B  B  B  B�B'�B0  B8  B@  BH  BP(�BX  B`  Bh  Bp(�Bx(�B�  B�  B�  B�  B�  B�{B�(�B�Q�B��B�{B�{B��B�{B�  B��B�  B�{B�  B�  B�  B�  B�  B��B�  B�  B��B�  B��B��B��B��
B��B��C  C
=C  C��C	��C
=C{C
=C
=C
=C{C
=C  C��C��C   C"  C#��C%�C'��C)��C,  C.  C/��C1��C4
=C6  C8  C:  C;��C>  C@  CA��CC��CE��CH
=CJ  CL  CN  CP  CR
=CT  CV{CX  CY��C\  C^  C`  Cb  Cd
=Cf
=Ch  Ci��Ck��Cn  Cp
=Cr  Ct  Cv
=Cx  Cy��C{��C~  C�C�  C���C�  C���C���C�  C���C���C�  C�  C�  C�  C�C�
=C�C�  C���C�  C�  C���C���C�  C�C�  C�C�
=C�  C�C�C���C�  C�  C���C�  C�C�C�C�C�  C�C�  C�  C�C�  C���C�  C���C���C���C���C�  C�  C���C���C�C�C�
=C�
=C���C���C���C���C���C�
=C�C�  C�C�  C�  C�  C�  C�  C�  C���C�  C�  C���C�  C�  C�  C���C���C�  C�C�C�C�
=C�  C�  C�C�
=C�
=C�C�  C�  C�  C�  C�C���C���C���C���C�  C�C���C���C���C�  C�C�C�  C�  C���C���C�  C�  C�  C�  C�C�C���C�  C�  C���C�  C�  C�D �D � D ��D� D�D��D  D� D�qD}qD��D� DD��D  D� D  D}qD��D	z�D	�qD
�DD� D  D�D�D��D  D� D�D�D  D}qD  D��D  D}qD�qD� D�D}qD�qDz�D  D�D�D��D  D}qD�D�D�D}qD  D�D�D}qD  D� D  D�D�D� D   D � D!  D!� D"  D"}qD"�qD#� D$�D$� D$�qD%� D&  D&}qD&�qD'}qD(  D(� D)  D)}qD)�qD*� D*�qD+}qD+�qD,� D-  D-� D.�D.� D.�qD/}qD/�qD0}qD1  D1��D2  D2� D3  D3}qD4  D4}qD4�qD5}qD6  D6� D7�D7� D7�qD8� D9  D9� D:  D:}qD;  D;� D;�qD<}qD<�qD=}qD>  D>� D?  D?��D@�D@� DA  DA}qDB  DB}qDB�qDC� DD  DD� DE�DE��DF  DF� DF�qDGz�DG�qDH� DI  DI}qDI�qDJ� DJ�qDK� DLDL��DM�DM� DM�qDN� DO  DO� DO�qDP� DQ�DQ� DR  DR� DS  DS� DT�DT�DU  DU� DVDV� DV�qDW� DX  DX� DY�DY� DY�qDZ}qD[  D[}qD[��D\}qD]  D]� D^  D^� D_  D_}qD`  D`��Da�Da� Da�qDb}qDc  Dc}qDc�qDd}qDd�qDe� De�qDf}qDg  Dg}qDg�qDh}qDh�qDi}qDi�qDj��Dk�Dk}qDl  Dl� Dm  Dm� Dn  Dn��Do  Do}qDo��Dp��Dq�Dq� Dr  Dr��DsDs��Dt  Dt� Du  Du� Dv�Dv� Dv�qDw}qDx  Dx��Dy  Dy}qDz  Dz}qDz��D{}qD|  D|}qD|�qD}� D~�D~� D�D� D�qD�AHD�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD���D�� D���D�=qD�}qD�� D�HD�@ D�}qD���D�  D�>�D�~�D��HD�HD�@ D�~�D���D�  D�AHD�� D���D���D�AHD���D��HD�  D�AHD���D�D�HD�AHD��HD��HD�HD�AHD�� D�� D�HD�AHD���D��HD�HD�B�D���D�� D���D�@ D�~�D��qD��qD�=qD�� D��HD�  D�@ D��HD��HD�  D�>�D�~�D��HD�  D�>�D�~�D���D���D�>�D�� D�� D�HD�AHD�� D�� D���D�>�D�� D�� D�HD�@ D��HD�� D���D�>�D�}qD�� D��D�@ D�� D���D�  D�@ D��HD���D�  D�AHD�� D��HD�HD�AHD�� D���D�  D�@ D�� D��HD�  D�AHD�� D�D��D�>�D�~�D�� D���D�@ D�~�D��HD�  D�@ D��HD���D�HD�@ D�� D�� D���D�>�D�� D��HD�HD�AHD�� D���D���D�=qD�� D���D���D�>�D�~�D���D�  D�>�D�~�D��qD��qD�@ D��HD���D���D�@ D�~�D�� D�HD�B�D��HD�� D�HD�AHD��HD�� D�  D�@ D�� D��qD�  D�AHD�~�D�� D���D�>�D�~�D���D�  D�@ D��HD�� D�HD�B�D�}qD���D���D�>�D�}qD���D���D�@ D�� D�� D��D�@ D�� D��HD�HD�>�D�~�D���D�  D�B�D�� D��qD��qD�@ D��HD�� D�HD�B�D��HD�� D���D�@ D���D�� D�  D�>�D�� D�� D���D�>�D�� D���D�  D�AHD�� D��qD��qD�@ D��HD�� D���D�@ D��HD��HD�  D�>�D�� D���D���D�>�D��HD��HD�  D�@ D�� D�� D�  D�@ D�~�D¾�D���D�@ DÀ D��HD�  D�@ DĀ D�� D�HD�AHDŁHD�� D���D�>�DƁHD��HD�HD�AHD�~�DǾ�D�  D�AHD�~�D�� D�  D�>�D�~�D�� D�  D�@ Dʀ D��HD�  D�>�Dˀ D�� D�  D�AHD̀ D�� D�  D�@ D�~�D�� D�  D�>�D�~�D�� D���D�@ Dπ D�� D�  D�AHD�~�D�� D�  D�AHDт�D�� D�  D�>�DҀ DҾ�D���D�@ DӀ D��HD�  D�AHDԀ D�� D�  D�@ DՀ D�� D�  D�@ D�~�D־�D���D�>�D�~�D�� D�  D�@ D؁HD�D�  D�>�DفHD��HD�  D�@ D�~�Dھ�D���D�@ Dۀ D�� D�HD�@ D܁HD�� D�  D�@ D�~�D�� D�HD�AHDށHD�D�  D�@ D߁HD��HD�  D�>�D��HD��HD���D�>�D� D�� D�HD�>�D�}qD⾸D�  D�>�D�~�D㾸D���D�>�D�~�D�� D���D�@ D� D�� D�  D�@ D�~�D�qD���D�>�D� D��HD�HD�AHD� D辸D���D�@ D�HD��HD�  D�AHD� D꾸D���D�@ D�~�D뾸D�  D�>�D�~�D�� D��qD�>�D� D�� D�HD�>�D�HD��HD�  D�@ D� D��HD�HD�AHD���D��HD���D�@ D�HD�D���D�AHD�HD�D�HD�@ D�~�D��HD��D�AHD�~�D��HD�HD�@ D�� D���D�  D�AHD�� D���D�  D�AHD�� D���D�  D�AHD�� D�� D��D�AHD�� D�� D�HD�@ D�}qG�O�?�?8Q�?u?���?\?�@�\@��@333@B�\@O\)@fff@z�H@��@��@�p�@�ff@�{@�(�@�ff@�\)@�p�@�@��@�(�Az�A��A{Az�A=qA�RA$z�A*=qA0  A4z�A9��A?\)ADz�AH��AN{ATz�AY��A]p�Ab�\AhQ�Amp�Aq�AuA{�A���A��HA���A��A�=qA�z�A��RA�G�A��
A�ffA�Q�A��\A��A�  A�=qA�z�A�
=A��A�(�A�ffA�G�A�(�A��RA���A��HA�A�  A�=qA��A�\)A�G�A˅A�ffAУ�A��HA���A�\)A��A�(�A�{A�  A��HA��A�\)A���A��
A�{A�  A�A�(�A�ffA�  A���A��
A�A��B ��BB�HB�Bz�BB�HB�
B��B	��B
�RB�
B��B��B�RB�B��B��B�\B�
B��B�B�HB�
B�B{B
=B(�BG�B=qB33B (�B!p�B"ffB#\)B$Q�B%��B&�\B'�B(��B*{B+33B,Q�B-p�B.�\B/�
B0��B1�B333B4z�B5G�B6�\B7�
B8��B9�B;
=B<Q�B=p�B>=qB?\)B@��BABB�RBC�BD��BE�BF�HBG�
BH��BI�BJ�HBK�BL��BM�BN�HBO�
BP��BQ�BS
=BS�
BT��BU�BW33BX  BX��BY�B[
=B\(�B\��B]�B^�RB_�
B`��BaBb�\Bc\)BdQ�BeG�Bf{Bf�HBg�
Bh��Bj{Bj�HBk�
Bm�BnffBo�Bp��Bq��Br�RBs�
Bu�Bv=qBw33Bx(�Byp�Bz�RB{�
B|��B}�B
=B�(�B��RB�33B�B�ffB�
=B���B�{B��RB�\)B�  B��\B�
=B��B�=qB��HB���B�(�B��RB�G�B�  B���B�33B�B�Q�B�
=B���B�=qB��RB�G�B��B��\B�33B�B�Q�B��HB��B�(�B��HB�p�B��B�z�B�
=B��B�  B�ffB���B��B�G�B��B�B�  B�{B�(�B�Q�B�z�B���B���B��HB���B��B�\)B��B��B��B�{B�=qB�Q�B���B���B��B�\)B��B�B�  B�=qB��\B���B��B�G�B��B�B�  B�Q�B���B��HB���B�33B�p�B�B�{B�ffB���B���B���B�G�B��B�B�{B�Q�B�z�B���B��HB�
=B�G�B��B��
B�  B�(�B�Q�B�z�B���B���B���B�33B�\)B���B��
B�  B�(�B�Q�B�ffB���B��HB��B�\)B��B��B�(�B�Q�B��\B���B�
=B�G�B���B��B�(�B�ffB���B���B�
=B�G�B��B��B�  B�=qB�z�B��RB���B�33B�\)B��B��
B�{B�=qB�z�B���B�
=B�\)B���B��
B�  B�Q�B�z�B���B���B��B�\)B���B��B�=qB�z�B���B�
=B�G�B��B��
B�  B�Q�B���B��HB�33B��B��
B�(�B��\B��HB�33B��B��
B�=qB��\B��HB�G�B���B��B�=qB��\B���B�33B��B��B�=qB��\B��HB�33B��B��B�(�B\B��HB�33BÙ�B��B�Q�Bģ�B�
=B�\)BŮB�{B�ffBƣ�B�
=B�\)BǮB�  B�Q�Bȣ�B���B�G�BɅB��
B�(�B�z�B���B�
=B�\)BˮB�  B�Q�B̏\B��HB�33B�p�B�B�{B�Q�BΏ\B���B�
=B�\)Bϙ�B��
B�(�B�ffBУ�B���B�33B�p�B�B�  B�=qB�z�B���B��B�\)BӮB�  B�Q�Bԣ�B���B�\)Bՙ�B�  B�Q�B֣�B���B�\)B׮B�{B�ffB���B�33BمB��B�Q�BڸRB��BۅB��B�ffBܸRB�33Bݙ�B�  B�z�B��HB�33Bߙ�B�  B�z�B��HB�G�B�B�(�B�\B�
=B�B��B�ffB��HB�\)B�B�(�B��B��B癚B�  B�z�B���B�p�B��
B�Q�B�RB�33B�B�(�B��B��B��B�{B�\B�
=B�B��B�ffB���B�G�B�B�=qB�RB�33B�B�(�B���B��B���B�  B�z�B���B�p�B��
B�Q�B��RB�33B�B�=qB��RB�33B�B�Q�B���B�\)B��
B�Q�B��RB�G�B��C �C ffC �C �C33Cz�C�RC��C33Cp�C�C�C33Cz�C�RC  C=qCz�C�RC  C33Cp�C�C�C(�CffC��C�
C{CQ�C�CC  C=qCz�C�RC	  C	=qC	z�C	�RC
  C
33C
p�C
�C
�C(�C\)C��C�HC�CffC�C�C(�CffC��C�HC�C\)C��C�HC(�C\)C��C��C{CQ�C��C�HC�CQ�C�\C��C{C\)C��C�HC�CQ�C�\C��C{CQ�C�\CC��C=qC�CC  CG�C�C�RC��C33Cz�CC  C=qCp�C�RC  C=qCz�C�RC  CG�C�C��C
=C=qC�\C�HC�C\)C��C�C33C�C��C
=CG�C�\C�C33Cz�CC 
=C Q�C ��C ��C!33C!z�C!�RC"
=C"\)C"�C"��C#=qC#�C#��C${C$ffC$�RC%  C%=qC%�C%�
C&(�C&z�C&C'
=C'Q�C'��C'��C(Q�C(�\C(�
C)(�C)�C)�
C*�C*ffC*�RC+
=C+ffC+�C+��C,G�C,��C,��C-33C-�C-��C.33C.z�C.C/{C/ffC/C0
=C0\)C0��C1
=C1Q�C1��C1��C2Q�C2�C2�C3G�C3��C4  C4G�C4��C4��C5Q�C5��C5�HC6=qC6��C6�C7=qC7�C7�C8G�C8�\C8�HC9=qC9��C9�HC:33C:�\C:�C;G�C;�\C;�HC<33C<�\C<�C==qC=�\C=�C>G�C>��C>�C?33C?�\C?�C@G�C@�\C@�HCA=qCA��CA�HCB33CB�CB�HCC=qCC�CC�
CD33CD�\CD�CE33CE�CE�HCF33CF�CF�
CG33CG�\CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@�o@�rG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aϥ�Aϩ�Aϥ�AϬAϮAϰ!Aϲ-Aϰ!Aϴ9AϸRAϴ9AϼjAϺ^AϾwAϾwAϸRAϸRAϸRAϺ^A���A���A�A�A���A���A�A���A�A�ƨA�ƨA�ȴA���A�ȴA�ȴA���A���A���A���A�AρA�I�A���A�+A���A��;A�hsA�r�A��A��A���A�n�A��A���A���A��TA���A��A��/A���A��7A�(�A�+A�%A�v�A�S�A�ƨA��+A�E�A��A�1A��A~v�A}K�Az�At{An�+AjI�Ae�PA^��A]�^A]XAZ1AU%ALbNAG�#AG��AGl�AGG�AG�AGG�AHAHbNAH-AF�yAD��AD�jAD�AB�\A@��A@ffA@ffA@�A?A>��A?`BA>��A<��A<ffA;ƨA:v�A8~�A7�-A7VA6��A6�RA6M�A5��A4�9A4Q�A2I�A1ƨA0ĜA.z�A,�A*�`A*1A(�RA(  A'O�A&(�A$�uA$1A#A#G�A"��A"{A!l�A �`A ��A ĜA ĜA ȴA �yA�;A�+AJA��A�7A7LAoA�/A�\A-A��AJA�AC�A�jA�A\)AoA�A��A�A�TA��A/A��A=qA�;A�FAXA��A�\AQ�A-A�A�hA`BAC�A�A�/A�9AjA�;A�A��A��AbNA=qA��A|�AXAȴA��A�FA�hA/A��A��AM�A�A�wAl�A
�/A
�A
M�A	��A	��A	t�A	%A�jA��An�A-A�
Ax�AhsA`BA�/A9XA�A�FA�AS�A
=A��A�jA��AbNA9XAJAx�A��A��AM�AƨA�hA\)A;dA&�A ȴA �DA I�@��;@�o@�5?@��h@�?}@��@��u@�ƨ@�@��+@�^5@�{@���@�/@���@���@��/@��@�z�@��F@��y@��@��@��@��@�V@�@�V@�j@�Z@�  @�F@��y@�ff@��@�h@�?}@�I�@�l�@�"�@��@�@��@ꟾ@�v�@�$�@�p�@�u@��
@�\)@���@�@��@��@�|�@�
=@�@�hs@�%@�z�@�b@���@ߕ�@�o@�n�@��@�@ݑh@ܼj@�9X@�b@�|�@�
=@ڏ\@��@�7L@���@�Q�@�ƨ@��y@��@�@Ցh@Ԭ@ҟ�@щ7@д9@�Z@��@�C�@�o@��@�ȴ@Χ�@·+@�^5@�@Ͳ-@�hs@�bN@˾w@��@�ff@ɩ�@ț�@�1'@��;@�t�@�+@��H@Ɨ�@���@őh@���@�9X@�ƨ@�33@���@�~�@�v�@�=q@��#@���@�/@�%@�z�@�C�@��R@���@��+@�E�@�@�hs@��@���@�Q�@�(�@���@��P@�
=@���@��H@�~�@��@���@�hs@�&�@���@���@��@�Q�@��P@��!@�$�@�J@��@���@�x�@�?}@��@��@��`@�bN@���@�"�@�~�@��@���@��@�j@�1'@��;@���@�ƨ@��F@��@�"�@�v�@��@��@�@��h@�7L@�V@���@��@��@�bN@�A�@��@�l�@�dZ@�S�@�@�~�@�$�@�J@�@�O�@�?}@��@��u@���@��
@��w@�K�@�+@��@�ȴ@�-@���@��@��/@�r�@�b@��m@��w@�"�@���@�E�@��-@�p�@���@�(�@��F@�l�@��@�~�@�M�@�-@���@���@�p�@���@�z�@�1@�@�E�@�@��^@��@�O�@�%@��/@�z�@��@�dZ@�dZ@�\)@�K�@�+@�"�@��@��@�n�@�=q@�$�@�@��@��@���@�p�@��@��@�Ĝ@���@�I�@��m@���@�\)@�33@��\@�V@�=q@��#@�p�@�G�@�&�@��`@��@�1@�ƨ@���@��@�C�@��@�ff@�{@���@��7@�X@��@���@�Q�@� �@�ƨ@���@�S�@��y@���@�ff@�E�@��@�O�@���@��/@���@��9@��D@�I�@�1@���@��@�
=@���@��!@���@��+@�V@���@���@�p�@�?}@�%@���@���@��D@�Q�@�9X@�9X@�(�@�b@�b@���@��F@�;d@���@���@��!@�~�@��@���@�p�@�?}@��@��@��@�  @��@l�@
=@~v�@}�-@}O�@|�/@|Z@{ƨ@{33@z��@z~�@z=q@y�@yX@x��@xbN@w�w@w�@wK�@vV@u?}@t(�@s�@r�@r~�@q�@q%@p��@p1'@o�@o�P@ol�@o+@o
=@n�@nV@n5?@n$�@n@m�h@m`B@mV@l(�@k�F@k�m@ko@j��@j=q@i��@i��@i�^@i7L@hĜ@hbN@hA�@h1'@h1'@hA�@h �@h1'@h1'@g��@gl�@g�@f�@f�+@f�+@f5?@e�-@d�/@d�@c��@cS�@co@b�!@b�@a�^@ahs@a�@`�`@`��@`�9@`1'@_��@^��@^ȴ@^�+@^$�@]�T@]��@]�h@]`B@\�j@\z�@\Z@\9X@\�@[�
@[��@[C�@["�@Z�\@Y�#@Y��@X��@X��@Xr�@W�;@W�w@W�w@W|�@W+@Vȴ@V��@V�+@Vv�@VE�@U@U�@T�j@T�@Tz�@S��@S�@SC�@R��@R~�@RM�@R-@R�@Q�@Q�#@Q�7@Q&�@Q%@P�9@Pr�@PQ�@O��@OK�@OK�@OK�@N��@N$�@N@M��@M�-@M�@M?}@L�@L�D@LI�@L1@K�
@K�F@K��@K��@K33@J~�@I�@Ix�@I%@H�9@H�u@H�@HbN@H1'@G�w@G|�@Gl�@GK�@G+@F�y@F�+@F{@E@EV@D�j@Dz�@D(�@C�m@C��@Ct�@B��@B-@A�^@Ahs@A&�@@Ĝ@@r�@@1'@?�;@>�@>ff@>E�@=�T@=�@=�@<��@<��@<�D@<(�@;��@;33@:^5@9�#@9&�@8Ĝ@8Q�@8 �@7�@7K�@7�@6�@6ȴ@6��@5@4��@4�@4Z@3ƨ@3dZ@3C�@3C�@3C�@333@333@3@2��@2��@2��@2^5@1�7@1�@0A�@0  @/�;@/�w@/�@/�P@/
=@.ȴ@.v�@.E�@.5?@.$�@.{@.@-�T@-@-�@-`B@-V@,�/@,��@,�@,Z@,(�@+�
@+t�@*�!@*M�@*-@)��@)��@)x�@)G�@)�@(��@(�9@(��@(Q�@(  @'��@'�@'\)@';d@'�@'�@'�@&��@&�R@&�+@&ff@&5?@%�@%�T@%��@%�@%?}@%V@$�@$9X@#�
@#��@#��@#�@#�@#t�@#dZ@#S�@#33@#@"�H@"�@"��@"��@"��@"M�@!��@!X@!G�@!7L@!�@!%@ ��@ Ĝ@ �9@ ��@ �@ r�@ b@�@��@|�@|�@l�@|�@+@�@�@V@5?@@@�-@�h@O�@�@�@V@�@�@�@�@�@�@�@�/@�/@�@�m@S�@o@�H@��@��@��@�\@n�@^5@�@7L@�`@��@�@bN@A�@ �@  @�@�w@�P@;d@�@��@E�@�h@`B@/@V@��@��@��@j@9X@9X@�@�m@��@�@��@��@M�@=q@-@�@��@��@x�@G�@&�@�`@Ĝ@�9@��@�u@r�@A�@�@��@l�@
=@��@E�@E�Aϧ�Aϧ�Aϟ�Aϩ�AϬAϩ�Aϧ�Aϩ�Aϧ�Aϣ�Aϣ�Aϩ�Aϩ�Aϩ�AϮAϬAϩ�AϮAϰ!AϮAϮAϴ9Aϲ-AϮAϲ-Aϲ-AϮAϴ9AϸRAϺ^A϶FAϸRA϶FAϴ9A϶FAϾwA���AϼjAϼjAϾwA���AϺ^Aϰ!A϶FA���AϾwAϼjAϾwA�A���AϾwAϼjA���A���AϼjAϾwAϾwA���AϼjA϶FAϺ^AϺ^AϸRA϶FA϶FAϼjAϺ^Aϴ9A϶FAϺ^AϺ^AϸRA϶FAϺ^AϼjA϶FAϺ^AϼjAϸRAϸRAϺ^AϼjAϺ^A϶FAϺ^A�A�A���AϾwA�A�A���AϼjAϾwA���AϾwAϾwA���A�ĜA���AϾwA�A�ĜA�A���A�ĜA�ƨA�A���A���A�ĜA�ĜA�A���A�A�ƨA���AϾwA�A�ĜA�A���A���A�A�AϾwAϾwA���A�A�ĜA���AϾwA�ĜA�A���AϾwA�A�ĜA���A���A�ĜA�ĜAϾwAϾwA���A�A�AϾwA���A�ĜA���A���A�A�ȴA�A�ĜA���A���A�ƨA���A�ĜA�ƨA�ĜA�ĜA���A�ƨA�A�A�ƨA���A�ȴA�ȴA�ƨA���A���A���A�ȴA���A���A���A���A���A���A�ƨA�ĜA�ĜA�ȴA�ȴA�ƨA�ƨA�ȴA���A���A�ȴA�ȴA���A���A���A�ƨA�ĜA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�A�ĜA���A�ĜAϺ^AϸRA�A���A�ƨAϩ�AυA�x�A�r�A�t�A�n�A�bNA�VA�G�A�I�A�I�A�C�A�+A�{A�VA���A��A��A��HA���Aδ9AΑhA�jA��A���Aͣ�A�Q�A�bAȕ�A�5?Aǉ7A���A�S�A�  A��TA���A�$�A��A���A��-A���A��\A��A�|�A�v�A�t�A�r�A�r�A�r�A�t�A�v�A�t�A�r�A�n�A�hsA�\)A�Q�A�A�A�"�A��A�oA���A���A��A�C�A��A��A��
A��+A�\)A��A�5?A��mA��A�K�A���A��A��+A��A���A��A��A��HA���A�t�A�;dA��A�A��A��yA��`A��
A��
A�A�A��wA��A���A���A��\A��PA��7A��+A��PA��A��A�t�A�hsA�M�A�/A���A���A���A��A�=qA��A���A�n�A��HA�I�A��+A��A�\)A�bA�  A��A��;A��#A��
A���A���A���A���A���A���A���A���A���A���A�ĜA��wA���A��jA��jA���A���A���A��\A��A�~�A�z�A�v�A�v�A�^5A�VA�"�A�VA�A��A��RA���A�ffA�G�A�5?A��A���A��A��A�~�A��A���A�1'A��;A���A�hsA�XA�G�A���A�~�A�|�A��\A�p�A���A��wA�A�A���A�&�A��A�
=A���A���A���A���A���A��uA��+A�jA�ffA�^5A�K�A�O�A�?}A�-A�"�A��mA�bNA���A�ƨA��!A�~�A�E�A�&�A��mA�ĜA��FA���A�`BA��A��`A���A��A�t�A�ffA�VA�K�A�=qA��A�1A�%A�A���A��A��;A�ƨA���A���A���A���A���A���A���A���A�t�A�;dA��jA�l�A��!A�K�A��A���A�|�A�Q�A�&�A�r�A��;A�\)A���A�ƨA�;dA�1A�M�A��yA��A�XA��/A���A���A�ƨA��jA�n�A�7LA�oA�M�A��A���A��^A�"�A�JA��hA�1A�ĜA���A�t�A�p�A�dZA�Q�A�E�A�9XA�+A��A�bA���A��;A���A��wA��A�ZA�VA��A��hA�/A�JA�1A��A��mA���A��A�?}A�"�A��HA��FA���A���A��+A�XA�E�A�"�A�bA�A��A��`A��#A���A���A��RA��-A��A���A���A���A��uA��DA��+A�z�A�l�A�hsA�dZA�`BA�XA�O�A�K�A�G�A�=qA�/A�JA���A�ĜA���A�v�A�bA���A��uA�K�A�ƨA�VA�+A��A���A���A�jA�JA���A���A� �A�;A�^A��Ap�A�A~�HA~�!A~�A~ZA~VA~I�A~=qA~�A}��A}��A}��A}p�A}?}A}�A|�`A|~�A|$�A{�mA{�FA{"�Az��Azz�Az �Ay��Ay�Ax��AxQ�Av{ArE�Ap�`Ap�DApr�ApM�Ao�Ao��Ao/An�/An�Am�-Am�Am33Al��Ak�wAj��AjZAi�
AidZAi?}Ai�Ah�yAh�RAg��Af��Ae?}Ac��Ab��Ab1'Aa�A_�A^��A^1'A^  A]�;A]�mA]��A]�#A]�#A]�A]�FA]�A]�FA]�FA]�A]��A]��A]��A]�A]\)A]XA]?}A]�A\ȴA\(�A[��A[VAY�AY;dAX�RAXA�AX{AW��AW�#AWl�AV�\AT��AQ�FAO�hAN�\AM��AM��AM�ALA�AKS�AJM�AI�#AH�+AG�AG�-AG��AG�hAG�7AG�PAG�PAG��AG��AG��AG��AG�hAG�7AG�AG|�AGp�AGl�AG`BAGS�AGO�AGK�AGK�AGO�AGG�AG?}AG?}AG7LAG/AG"�AG�AG�AG
=AG
=AG
=AGVAG�AGG�AGp�AG�AG�AG��AG�^AH�AH(�AH=qAHE�AHQ�AHZAH^5AH^5AH^5AHjAHn�AHn�AHZAH-AHJAH1AHAG��AG�TAG��AGoAF{AE�AEO�AEoAEAD�yAD�/AD��AD��AD�RAD�9AD�jAD�jAD�jAD�RAD�RAD�RAD�9AD�uAD5?AC�TAC�PAB�9ABM�AB^5AB(�AAƨAA
=A@�jA@�+A@v�A@n�A@bNA@r�A@v�A@jA@bNA@ZA@ZA@VA@M�A@I�A@v�A@�A@��A@��A@��A@�`A@�yA@�yA@�/A@ȴA@�9A@9XA?33A>z�A>�A>�+A>^5A>A�A>��A>��A?�A?7LA??}A?K�A?l�A?�PA?��A?�TA?�^A?K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                   Aϥ�Aϩ�Aϥ�AϬAϮAϰ!Aϲ-Aϰ!Aϴ9AϸRAϴ9AϼjAϺ^AϾwAϾwAϸRAϸRAϸRAϺ^A���A���A�A�A���A���A�A���A�A�ƨA�ƨA�ȴA���A�ȴA�ȴA���A���A���A���A�AρA�I�A���A�+A���A��;A�hsA�r�A��A��A���A�n�A��A���A���A��TA���A��A��/A���A��7A�(�A�+A�%A�v�A�S�A�ƨA��+A�E�A��A�1A��A~v�A}K�Az�At{An�+AjI�Ae�PA^��A]�^A]XAZ1AU%ALbNAG�#AG��AGl�AGG�AG�AGG�AHAHbNAH-AF�yAD��AD�jAD�AB�\A@��A@ffA@ffA@�A?A>��A?`BA>��A<��A<ffA;ƨA:v�A8~�A7�-A7VA6��A6�RA6M�A5��A4�9A4Q�A2I�A1ƨA0ĜA.z�A,�A*�`A*1A(�RA(  A'O�A&(�A$�uA$1A#A#G�A"��A"{A!l�A �`A ��A ĜA ĜA ȴA �yA�;A�+AJA��A�7A7LAoA�/A�\A-A��AJA�AC�A�jA�A\)AoA�A��A�A�TA��A/A��A=qA�;A�FAXA��A�\AQ�A-A�A�hA`BAC�A�A�/A�9AjA�;A�A��A��AbNA=qA��A|�AXAȴA��A�FA�hA/A��A��AM�A�A�wAl�A
�/A
�A
M�A	��A	��A	t�A	%A�jA��An�A-A�
Ax�AhsA`BA�/A9XA�A�FA�AS�A
=A��A�jA��AbNA9XAJAx�A��A��AM�AƨA�hA\)A;dA&�A ȴA �DA I�@��;@�o@�5?@��h@�?}@��@��u@�ƨ@�@��+@�^5@�{@���@�/@���@���@��/@��@�z�@��F@��y@��@��@��@��@�V@�@�V@�j@�Z@�  @�F@��y@�ff@��@�h@�?}@�I�@�l�@�"�@��@�@��@ꟾ@�v�@�$�@�p�@�u@��
@�\)@���@�@��@��@�|�@�
=@�@�hs@�%@�z�@�b@���@ߕ�@�o@�n�@��@�@ݑh@ܼj@�9X@�b@�|�@�
=@ڏ\@��@�7L@���@�Q�@�ƨ@��y@��@�@Ցh@Ԭ@ҟ�@щ7@д9@�Z@��@�C�@�o@��@�ȴ@Χ�@·+@�^5@�@Ͳ-@�hs@�bN@˾w@��@�ff@ɩ�@ț�@�1'@��;@�t�@�+@��H@Ɨ�@���@őh@���@�9X@�ƨ@�33@���@�~�@�v�@�=q@��#@���@�/@�%@�z�@�C�@��R@���@��+@�E�@�@�hs@��@���@�Q�@�(�@���@��P@�
=@���@��H@�~�@��@���@�hs@�&�@���@���@��@�Q�@��P@��!@�$�@�J@��@���@�x�@�?}@��@��@��`@�bN@���@�"�@�~�@��@���@��@�j@�1'@��;@���@�ƨ@��F@��@�"�@�v�@��@��@�@��h@�7L@�V@���@��@��@�bN@�A�@��@�l�@�dZ@�S�@�@�~�@�$�@�J@�@�O�@�?}@��@��u@���@��
@��w@�K�@�+@��@�ȴ@�-@���@��@��/@�r�@�b@��m@��w@�"�@���@�E�@��-@�p�@���@�(�@��F@�l�@��@�~�@�M�@�-@���@���@�p�@���@�z�@�1@�@�E�@�@��^@��@�O�@�%@��/@�z�@��@�dZ@�dZ@�\)@�K�@�+@�"�@��@��@�n�@�=q@�$�@�@��@��@���@�p�@��@��@�Ĝ@���@�I�@��m@���@�\)@�33@��\@�V@�=q@��#@�p�@�G�@�&�@��`@��@�1@�ƨ@���@��@�C�@��@�ff@�{@���@��7@�X@��@���@�Q�@� �@�ƨ@���@�S�@��y@���@�ff@�E�@��@�O�@���@��/@���@��9@��D@�I�@�1@���@��@�
=@���@��!@���@��+@�V@���@���@�p�@�?}@�%@���@���@��D@�Q�@�9X@�9X@�(�@�b@�b@���@��F@�;d@���@���@��!@�~�@��@���@�p�@�?}@��@��@��@�  @��@l�@
=@~v�@}�-@}O�@|�/@|Z@{ƨ@{33@z��@z~�@z=q@y�@yX@x��@xbN@w�w@w�@wK�@vV@u?}@t(�@s�@r�@r~�@q�@q%@p��@p1'@o�@o�P@ol�@o+@o
=@n�@nV@n5?@n$�@n@m�h@m`B@mV@l(�@k�F@k�m@ko@j��@j=q@i��@i��@i�^@i7L@hĜ@hbN@hA�@h1'@h1'@hA�@h �@h1'@h1'@g��@gl�@g�@f�@f�+@f�+@f5?@e�-@d�/@d�@c��@cS�@co@b�!@b�@a�^@ahs@a�@`�`@`��@`�9@`1'@_��@^��@^ȴ@^�+@^$�@]�T@]��@]�h@]`B@\�j@\z�@\Z@\9X@\�@[�
@[��@[C�@["�@Z�\@Y�#@Y��@X��@X��@Xr�@W�;@W�w@W�w@W|�@W+@Vȴ@V��@V�+@Vv�@VE�@U@U�@T�j@T�@Tz�@S��@S�@SC�@R��@R~�@RM�@R-@R�@Q�@Q�#@Q�7@Q&�@Q%@P�9@Pr�@PQ�@O��@OK�@OK�@OK�@N��@N$�@N@M��@M�-@M�@M?}@L�@L�D@LI�@L1@K�
@K�F@K��@K��@K33@J~�@I�@Ix�@I%@H�9@H�u@H�@HbN@H1'@G�w@G|�@Gl�@GK�@G+@F�y@F�+@F{@E@EV@D�j@Dz�@D(�@C�m@C��@Ct�@B��@B-@A�^@Ahs@A&�@@Ĝ@@r�@@1'@?�;@>�@>ff@>E�@=�T@=�@=�@<��@<��@<�D@<(�@;��@;33@:^5@9�#@9&�@8Ĝ@8Q�@8 �@7�@7K�@7�@6�@6ȴ@6��@5@4��@4�@4Z@3ƨ@3dZ@3C�@3C�@3C�@333@333@3@2��@2��@2��@2^5@1�7@1�@0A�@0  @/�;@/�w@/�@/�P@/
=@.ȴ@.v�@.E�@.5?@.$�@.{@.@-�T@-@-�@-`B@-V@,�/@,��@,�@,Z@,(�@+�
@+t�@*�!@*M�@*-@)��@)��@)x�@)G�@)�@(��@(�9@(��@(Q�@(  @'��@'�@'\)@';d@'�@'�@'�@&��@&�R@&�+@&ff@&5?@%�@%�T@%��@%�@%?}@%V@$�@$9X@#�
@#��@#��@#�@#�@#t�@#dZ@#S�@#33@#@"�H@"�@"��@"��@"��@"M�@!��@!X@!G�@!7L@!�@!%@ ��@ Ĝ@ �9@ ��@ �@ r�@ b@�@��@|�@|�@l�@|�@+@�@�@V@5?@@@�-@�h@O�@�@�@V@�@�@�@�@�@�@�@�/@�/@�@�m@S�@o@�H@��@��@��@�\@n�@^5@�@7L@�`@��@�@bN@A�@ �@  @�@�w@�P@;d@�@��@E�@�h@`B@/@V@��@��@��@j@9X@9X@�@�m@��@�@��@��@M�@=q@-@�@��@��@x�@G�@&�@�`@Ĝ@�9@��@�u@r�@A�@�@��@l�@
=@��@E�G�O�Aϧ�Aϧ�Aϟ�Aϩ�AϬAϩ�Aϧ�Aϩ�Aϧ�Aϣ�Aϣ�Aϩ�Aϩ�Aϩ�AϮAϬAϩ�AϮAϰ!AϮAϮAϴ9Aϲ-AϮAϲ-Aϲ-AϮAϴ9AϸRAϺ^A϶FAϸRA϶FAϴ9A϶FAϾwA���AϼjAϼjAϾwA���AϺ^Aϰ!A϶FA���AϾwAϼjAϾwA�A���AϾwAϼjA���A���AϼjAϾwAϾwA���AϼjA϶FAϺ^AϺ^AϸRA϶FA϶FAϼjAϺ^Aϴ9A϶FAϺ^AϺ^AϸRA϶FAϺ^AϼjA϶FAϺ^AϼjAϸRAϸRAϺ^AϼjAϺ^A϶FAϺ^A�A�A���AϾwA�A�A���AϼjAϾwA���AϾwAϾwA���A�ĜA���AϾwA�A�ĜA�A���A�ĜA�ƨA�A���A���A�ĜA�ĜA�A���A�A�ƨA���AϾwA�A�ĜA�A���A���A�A�AϾwAϾwA���A�A�ĜA���AϾwA�ĜA�A���AϾwA�A�ĜA���A���A�ĜA�ĜAϾwAϾwA���A�A�AϾwA���A�ĜA���A���A�A�ȴA�A�ĜA���A���A�ƨA���A�ĜA�ƨA�ĜA�ĜA���A�ƨA�A�A�ƨA���A�ȴA�ȴA�ƨA���A���A���A�ȴA���A���A���A���A���A���A�ƨA�ĜA�ĜA�ȴA�ȴA�ƨA�ƨA�ȴA���A���A�ȴA�ȴA���A���A���A�ƨA�ĜA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�A�ĜA���A�ĜAϺ^AϸRA�A���A�ƨAϩ�AυA�x�A�r�A�t�A�n�A�bNA�VA�G�A�I�A�I�A�C�A�+A�{A�VA���A��A��A��HA���Aδ9AΑhA�jA��A���Aͣ�A�Q�A�bAȕ�A�5?Aǉ7A���A�S�A�  A��TA���A�$�A��A���A��-A���A��\A��A�|�A�v�A�t�A�r�A�r�A�r�A�t�A�v�A�t�A�r�A�n�A�hsA�\)A�Q�A�A�A�"�A��A�oA���A���A��A�C�A��A��A��
A��+A�\)A��A�5?A��mA��A�K�A���A��A��+A��A���A��A��A��HA���A�t�A�;dA��A�A��A��yA��`A��
A��
A�A�A��wA��A���A���A��\A��PA��7A��+A��PA��A��A�t�A�hsA�M�A�/A���A���A���A��A�=qA��A���A�n�A��HA�I�A��+A��A�\)A�bA�  A��A��;A��#A��
A���A���A���A���A���A���A���A���A���A���A�ĜA��wA���A��jA��jA���A���A���A��\A��A�~�A�z�A�v�A�v�A�^5A�VA�"�A�VA�A��A��RA���A�ffA�G�A�5?A��A���A��A��A�~�A��A���A�1'A��;A���A�hsA�XA�G�A���A�~�A�|�A��\A�p�A���A��wA�A�A���A�&�A��A�
=A���A���A���A���A���A��uA��+A�jA�ffA�^5A�K�A�O�A�?}A�-A�"�A��mA�bNA���A�ƨA��!A�~�A�E�A�&�A��mA�ĜA��FA���A�`BA��A��`A���A��A�t�A�ffA�VA�K�A�=qA��A�1A�%A�A���A��A��;A�ƨA���A���A���A���A���A���A���A���A�t�A�;dA��jA�l�A��!A�K�A��A���A�|�A�Q�A�&�A�r�A��;A�\)A���A�ƨA�;dA�1A�M�A��yA��A�XA��/A���A���A�ƨA��jA�n�A�7LA�oA�M�A��A���A��^A�"�A�JA��hA�1A�ĜA���A�t�A�p�A�dZA�Q�A�E�A�9XA�+A��A�bA���A��;A���A��wA��A�ZA�VA��A��hA�/A�JA�1A��A��mA���A��A�?}A�"�A��HA��FA���A���A��+A�XA�E�A�"�A�bA�A��A��`A��#A���A���A��RA��-A��A���A���A���A��uA��DA��+A�z�A�l�A�hsA�dZA�`BA�XA�O�A�K�A�G�A�=qA�/A�JA���A�ĜA���A�v�A�bA���A��uA�K�A�ƨA�VA�+A��A���A���A�jA�JA���A���A� �A�;A�^A��Ap�A�A~�HA~�!A~�A~ZA~VA~I�A~=qA~�A}��A}��A}��A}p�A}?}A}�A|�`A|~�A|$�A{�mA{�FA{"�Az��Azz�Az �Ay��Ay�Ax��AxQ�Av{ArE�Ap�`Ap�DApr�ApM�Ao�Ao��Ao/An�/An�Am�-Am�Am33Al��Ak�wAj��AjZAi�
AidZAi?}Ai�Ah�yAh�RAg��Af��Ae?}Ac��Ab��Ab1'Aa�A_�A^��A^1'A^  A]�;A]�mA]��A]�#A]�#A]�A]�FA]�A]�FA]�FA]�A]��A]��A]��A]�A]\)A]XA]?}A]�A\ȴA\(�A[��A[VAY�AY;dAX�RAXA�AX{AW��AW�#AWl�AV�\AT��AQ�FAO�hAN�\AM��AM��AM�ALA�AKS�AJM�AI�#AH�+AG�AG�-AG��AG�hAG�7AG�PAG�PAG��AG��AG��AG��AG�hAG�7AG�AG|�AGp�AGl�AG`BAGS�AGO�AGK�AGK�AGO�AGG�AG?}AG?}AG7LAG/AG"�AG�AG�AG
=AG
=AG
=AGVAG�AGG�AGp�AG�AG�AG��AG�^AH�AH(�AH=qAHE�AHQ�AHZAH^5AH^5AH^5AHjAHn�AHn�AHZAH-AHJAH1AHAG��AG�TAG��AGoAF{AE�AEO�AEoAEAD�yAD�/AD��AD��AD�RAD�9AD�jAD�jAD�jAD�RAD�RAD�RAD�9AD�uAD5?AC�TAC�PAB�9ABM�AB^5AB(�AAƨAA
=A@�jA@�+A@v�A@n�A@bNA@r�A@v�A@jA@bNA@ZA@ZA@VA@M�A@I�A@v�A@�A@��A@��A@��A@�`A@�yA@�yA@�/A@ȴA@�9A@9XA?33A>z�A>�A>�+A>^5A>A�A>��A>��A?�A?7LA??}A?K�A?l�A?�PA?��A?�TA?�^A?K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                   ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�FB�FB��B�B�B�FB��B�zB�FB�zB�FB�B�zB�zB��B�zB��B�zB�FB�B��B�B�LB��B��B��B�zB�zB�zB�FB�FB�FB�zB�zB�B��B�tB�?B�FB�3BϫB�B�B	��B
\�B
poB
��B
��B
��B
� B
�B
�KB
�,B
��B
�xB
��B
p�B
a�B
QB
E�B
&�B
DB
�B
B	��B	��B
 �B
�B
�B	� B	�;B	�aB	�pB	��B	�)B	��B	�kB	�$B	tB	c�B	_�B	U2B	F�B	�B	B	$B	�B	'�B	0�B	B�B	j�B	�;B	�B	�^B	��B	��B	�aB	��B	��B	��B	�
B	��B	�ZB	�PB
�B
9�B
:^B
;�B
A B
EB
<jB
:�B
9XB
8RB
7LB
9XB
@�B
C�B
@OB
8B
-�B
*�B
"4B
$�B
~B
$�B
�B
YB
\B
(B
B

�B

�B

=B

=B
~B
oB
�B
�B
�B
!�B
!�B
-B
.B
!�B
VB
�B
B
�B
�B
+B
�B
~B
"�B
+�B
0�B
49B
4B
6zB
4B
5B
8B
@B
;dB
=�B
?�B
?B
EB
D�B
A B
?B
?}B
@�B
B'B
C�B
D�B
EB
FtB
GB
GEB
G�B
GzB
F?B
HB
JXB
I�B
G�B
H�B
J�B
I�B
L0B
J�B
H�B
J#B
E�B
B�B
C-B
D3B
B[B
A�B
B�B
A�B
B'B
B�B
A�B
A B
@OB
@�B
?}B
@�B
?}B
?}B
>�B
?�B
>�B
?HB
>BB
<�B
<jB
>BB
=�B
>wB
>�B
?B
?}B
@�B
?}B
>�B
?HB
>BB
=<B
<B
=�B
;�B
<�B
;�B
=qB
<6B
<�B
;�B
;�B
;dB
9�B
:*B
8�B
9�B
9�B
9$B
7�B
8B
7�B
8�B
9�B
8�B
:^B
:^B
:�B
:*B
9$B
8�B
8�B
8B
7�B
7�B
4�B
4�B
1[B
2-B
0�B
0�B
0�B
0�B
/�B
0!B
0UB
1[B
1'B
0UB
0UB
/OB
/OB
/�B
.}B
-CB
-B
-CB
-B
,�B
,=B
,qB
+�B
+kB
)�B
(�B
'�B
&�B
$tB
"�B
 �B
!B
VB
IB
�B
�B
qB
qB
�B
�B
�B
B
�B
�B
 �B
 �B
 �B
"4B
"hB
!�B
"hB
 �B
�B
 'B
�B
�B
B
xB
qB
eB
SB
�B
�B
�B
1B
�B
�B
�B
�B
�B
_B
_B
+B
�B
YB
�B
$B
�B
�B
�B
�B
�B
�B
�B
�B
SB
SB
B
�B
�B
�B
�B
�B
qB
	B
qB
B
�B
B
B
qB
�B
xB
B
�B
�B
�B
qB
qB
	B
qB
	B
�B
	B
�B
qB
	B
	B
qB
B
qB
=B
	B
=B
�B
kB
7B
kB
kB
�B
eB
eB
�B
eB
eB
�B
�B
+B
�B
�B
SB
FB
FB
FB
@B
uB
�B
@B
B
�B
B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
YB
YB
_B
+B
+B
�B
�B
�B
�B
�B
eB
B
�B
�B
�B
�B
B
B
�B
�B
B
�B
	B
�B
qB
	B
�B
CB
B
�B
�B
CB
�B
~B
IB
�B
�B
VB
!B
 'B
 �B
 �B
 �B
 �B
 'B
�B
 �B
�B
 �B
"�B
"�B
"�B
#nB
#:B
#�B
#�B
#�B
$@B
&B
&B
&B
&�B
'�B
(�B
(�B
(XB
(�B
(�B
)*B
)*B
)�B
*eB
*eB
*�B
+B
+kB
+kB
+�B
+kB
,=B
,�B
-CB
-B
-�B
.�B
.}B
.}B
/OB
/�B
/�B
/�B
0UB
1'B
1[B
2-B
1�B
1�B
1�B
1�B
1�B
1�B
2aB
2-B
2aB
2aB
2�B
3hB
3�B
3�B
3hB
4B
4�B
5?B
5tB
5tB
6FB
6�B
7LB
6�B
7B
7�B
7�B
8RB
8�B
8�B
9XB
9�B
9�B
9�B
9�B
9�B
:�B
:�B
;0B
;�B
<6B
<6B
<jB
<�B
=B
=<B
=qB
=�B
>�B
?B
?HB
?�B
?�B
?�B
>BB
>wB
>wB
>�B
>BB
>wB
>wB
?B
?B
>�B
@B
@�B
@OB
@�B
@�B
A�B
AUB
@�B
A�B
B'B
B�B
CaB
C�B
D3B
D3B
D3B
FB
GB
GB
HKB
I�B
I�B
JXB
J�B
I�B
HKB
IB
H�B
IRB
IRB
I�B
J�B
J�B
J�B
J�B
K^B
K^B
K�B
L0B
LdB
K�B
LdB
LdB
MB
M6B
NB
M6B
MjB
OB
M�B
N<B
NpB
N�B
N�B
O�B
O�B
PB
P}B
P�B
P}B
PHB
Q�B
P�B
QB
Q�B
Q�B
Q�B
R B
R B
QNB
RTB
R�B
R�B
S�B
S�B
S�B
S&B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U2B
VB
U�B
VB
VB
VmB
V9B
V�B
V9B
V�B
W?B
V�B
W?B
W?B
V�B
WsB
W?B
W?B
WsB
X�B
XEB
XEB
Y�B
YB
Y�B
Z�B
ZB
ZQB
[�B
[#B
\]B
[#B
[�B
[�B
[WB
[�B
[�B
\�B
[�B
[�B
\�B
]/B
\�B
^B
]�B
]�B
^5B
]�B
^jB
^B
^B
^�B
^�B
_;B
^jB
_B
`B
`B
_�B
^�B
`�B
`vB
_�B
_�B
`B
`vB
`�B
aB
aB
`�B
`�B
aB
`�B
`�B
`vB
a|B
bB
b�B
c B
c�B
c�B
c B
c�B
cTB
c�B
d&B
c�B
c�B
c�B
d&B
d&B
d�B
d�B
d�B
e`B
e�B
e,B
e�B
e�B
f2B
ffB
f�B
ffB
f�B
f�B
g8B
g�B
g�B
gmB
gmB
h
B
h
B
g�B
h�B
iDB
i�B
i�B
i�B
iyB
i�B
jKB
j�B
j�B
kQB
k�B
k�B
lWB
lWB
l�B
l�B
l�B
l�B
l�B
l�B
n/B
n/B
ncB
o5B
pB
o�B
pB
pB
oiB
p;B
o�B
pB
p;B
pB
pB
poB
qB
qB
r|B
rGB
rGB
rGB
r|B
rGB
r�B
sB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
tB
tTB
t�B
u%B
tTB
t�B
tB
u%B
uZB
u�B
v+B
u�B
v+B
u�B
v+B
v�B
v�B
v`B
v�B
w�B
w�B
wfB
w�B
xB
xB
w�B
w2B
w2B
w�B
x8B
xlB
x�B
xlB
w�B
xB
x�B
y>B
y	B
y	B
zB
y�B
zB
zB
zDB
zDB
zxB
{B
zxB
z�B
{�B
{B
z�B
{JB
{�B
{B
|B
|�B
|PB
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}VB
}�B
}VB
}�B
}VB
}�B
|�B
}�B
}�B
}�B
~�B
~]B
}�B
~]B
~�B
.B
cB
�B
~�B
~�B
�B
�B
�B
�B
~�B
cB
.B
cB
~�B
~�B
�4B
��B
�4B
�4B
�4B
� B
��B
�;B
�;B
��B
�;B
��B
�;B
�B
��B
�oB
��B
��B
�uB
�uB
�uB
��B
��B
�B
�B
�oB
��B
�oB
��B
��B
�oB
�B
��B
��B
�B
�B
�{B
��B
��B
��B
�uB
�{B
��B
��B
�SB
��B
�%B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�+B
��B
�+B
��B
��B
�+B
��B
�_B
�+B��B��B��B��B�B�FB��B�B�zB��B�B�nB�zB��B�?B�FB��B�B�?B�zB��B�tB��B��B�zB��B�LB�B��B��B�RB��B�tB��B��B�9B��B�B�B��B�tB��B��B�zB��B��B��B�B��B��B�B�LB��B��B��B�LB��B�tB�zB�B��B��B��B��B��B�B��B��B�zB�zB��B��B��B��B��B��B�FB�tB��B�B�FB�tB�FB��B��B�zB��B��B��B�zB�FB�LB��B�LB�zB�LB��B�B�B�LB��B��B��B��B��B��B�B�B�RB��B�FB�zB��B�RB�LB��B�LB��B�zB�B�FB�LB��B��B��B�LB��B�B�zB��B��B��B�tB��B�B��B�zB��B�LB��B�B��B��B�LB�B��B�B��B�FB�B�zB��B�FB��B�FB�LB��B��B��B�LB�zB��B�B�B��B��B��B��B�tB�nB�zB�FB��B�FB�B�FB�B��B�tB��B��B�?B�B�B�LB��B��B��B�B�B��B�?B��B��B�LB��B�B�tB�B�LB�FB�B��B�B�zB�?B��B�zB��B�FB�B�B��B��B��B��B��B��B�tB�FB�B�nB��B�tB��B��B�9B�B��B��B�B�LB�?B��B��B��B��B�nB��B�'B�EBǮB�tB��BʌB��B�NB�BΥB�B�mBںB֡BیB��B�BچB�B��B��B�yB	 �B�JB	B	S�B	�OB	��B	��B	��B	��B	�)B	��B
�B
n�B
ZQB
[�B
[�B
[#B
\)B
]dB
^�B
^�B
^�B
_B
_pB
_�B
_pB
^B
\�B
\)B
[�B
\�B
]�B
_pB
_�B
aB
f�B
a�B
bB
f2B
l�B
u�B
x8B
zxB
}VB
x�B
��B
zB
�GB
�DB
��B
�oB
��B
��B
��B
��B
��B
��B
��B
��B
�kB
��B
��B
��B
�\B
��B
��B
�VB
�(B
�@B
� B
�B
��B
� B
��B
�B
��B
��B
�oB
�B
��B
�bB
��B
�.B
� B
�.B
�SB
�@B
��B
��B
��B
��B
��B
��B
�4B
��B
�$B
ǮB
�EB
�B
��B
�B
��B
�B
�B
�NB
�B
�ZB
��B
� B
�TB
�B
�B
�B
��B
�B
�ZB
�&B
�ZB
�ZB
�B
�|B
��B
�B
�B
�pB
�B
ޞB
��B
�;B
�5B
�pB
�B
��B
��B
�]B
�pB
�vB
�dB
�B
�B
ٴB
ӏB
�TB
ӏB
�)B
�B
��B
�B
��B
��B
ѷB
�B
�mB
��B
�B
�tB
��B
�tB
��B
�B
��B
ߤB
	B
��B
�?B
�B
��B
��B
��B
�qB
�qB
��B
��B
�*B
��B
��B
��B
�OB
�B
��B
��B
��B
��B
�FB
��B
�1B
�VB
�1B
�MB
��B
�4B
}�B
.B
�B
}"B
|PB
z�B
u�B
s�B
s�B
qAB
poB
sMB
u%B
n�B
l�B
l�B
n�B
k�B
n�B
n/B
n/B
ffB
dZB
d�B
cTB
b�B
aHB
a|B
c B
]dB
d�B
NpB
d&B
Z�B
b�B
_�B
YB
U�B
Q�B
hsB
j�B
[�B
e�B
QB
5tB
/�B
OB
T�B
VmB
K)B
B�B
5�B
0UB
1'B
0UB
33B
��B
^�B
:*B
4nB
%�B
!�B
C�B
gmB
%�B
!-B
�B
FB
�B
B
�B
PB
xB

�B

=B
xB

	B
~B
	B
1B
+B
B
�B
MB
�B
B
_B	�.B	�VB
SB	��B
MB
1B	��B	��B
B
GB	��B	��B	�(B	�.B	��B	�VB	��B	��B	��B	�B	�xB	��B	�B	�PB	�B	��B	��B	��B	��B	��B
�B
�B
{B
B
oB
�B
B
B
 iB
 iB	��B
�B	��B
�B
�B
bB
�B
�B	�B	�B	��B	�B
�B	�B	�DB	�B	��B	��B	� B	�iB	�B	��B	�)B	�B	ٴB	�?B	�QB	خB	�#B	�yB	��B	��B	уB	�}B	бB	ҽB	��B	��B	�}B	͟B	�)B	��B	ɺB	̘B	��B	�B	��B	�<B	ȴB	�mB	�#B	��B	�9B	�B	�aB	�	B	�TB	��B	�-B	��B	��B	�zB	��B	��B	�zB	��B	�=B	�kB	��B	��B	��B	��B	��B	�9B	�B	��B	�bB	��B	�$B	��B	��B	�B	��B	��B	~�B	��B	��B	yrB	k�B	o B	kQB	d�B	b�B	c�B	dZB	i�B	d�B	d�B	aHB	b�B	b�B	a|B	b�B	`�B	e`B	`�B	\]B	Z�B	aHB	Z�B	e�B	XyB	WsB	h�B	V9B	N�B	E�B	@�B	<jB	7B	CaB	R B	UgB	m�B	9XB	&�B	$tB	�B	"�B	,B	�B	�B	FB	%B	�B	4B	�B	oB	uB	�B	uB	�B	B	�B	eB	�B	xB	B	OB	~B	!B	�B	 �B	 �B	#�B	$tB	#nB	*�B	*eB	-B	.B	0!B	1[B	0!B	0UB	3hB	/�B	2�B	5�B	7LB	?�B	L�B	R�B	T�B	UgB	V�B	u%B	w2B	xlB	|�B	}"B	}VB	~]B	�iB	�uB	��B	�B	��B	�B	�uB	��B	��B	��B	�B	��B	��B	B	�B	�$B	��B	�XB	��B	��B	��B	�*B	�B	�}B	�qB	�wB	�BB	�HB	��B	��B	��B	��B	�mB	��B	��B	�mB	�zB	��B	�9B	�-B	��B	�[B	��B	�pB	��B	уB	бB	҉B	�aB	֡B	�9B	�B	՛B	�,B	�gB	�9B	�}B	��B	��B	�B	�`B	��B	��B	��B	��B	��B	�|B	�(B
�B	�B	��B	�+B	��B	�B	�WB
�B
\B
{B
�B
=B
qB
%zB
�B
+6B
49B
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                   B�DB�oB��B�B�B�KB��B�gB�DB��B�*B�1B�}B��B��B��B��B��B�AB�)B��B�/B�lB��B��B��B��B�}B��B�SB�TB�hB��B�rB�)B��B��B��B��BŕB� B�QB	�B	�fB
h�B
�B
�*B
�'B
ŢB
�PB
�B
�B
�3B
��B
��B
�IB
z�B
�B
}%B
j�B
3xB
YB
GB
	8B
 )B	��B
�B
�B
DB	� B	��B	��B	רB	ݡB	�_B	�B	��B	�jB	w~B	e�B	kLB	gzB	b�B	-/B	�B	�B	�B	(B	/�B	@�B	i�B	�HB	��B	��B	�B	�B	��B	��B	νB	԰B	�5B	�B	��B	��B
�B
@?B
;`B
>?B
E�B
KKB
?UB
=B
:6B
8�B
8�B
;�B
C�B
E�B
F�B
:7B
1�B
2�B
*iB
(�B
 �B
)B
�B
�B
�B
�B
#B
�B
�B
�B
jB
�B
8B
B
0B
�B
!�B
!�B
0�B
2}B
#�B
 NB
�B
1B
gB
�B
XB
B
�B
"B
,YB
2�B
6?B
6�B
8{B
5B
5B
8�B
B�B
<oB
>�B
AMB
@B
G}B
E�B
A�B
@|B
AQB
A�B
C
B
D�B
E�B
FWB
G*B
G�B
G�B
H�B
HB
GmB
I�B
K�B
K�B
G�B
JzB
K�B
KyB
MKB
K�B
KB
L�B
F�B
C�B
D�B
E{B
CB
B�B
D
B
B�B
CiB
D�B
C;B
A�B
A�B
A9B
@�B
BB
@�B
?�B
?�B
@�B
@%B
@�B
>�B
=#B
>WB
@vB
?B
?MB
?dB
?�B
@�B
AMB
?�B
?hB
@B
>�B
>B
>2B
?�B
=4B
>B
=tB
>7B
=B
=B
<B
<�B
<JB
:�B
;�B
:DB
;YB
:�B
9�B
8DB
9*B
9gB
:B
:�B
9PB
:�B
;*B
;qB
:�B
9@B
9$B
8�B
8�B
91B
9$B
6.B
6�B
3VB
3�B
2B
1�B
26B
1(B
0<B
0�B
0�B
2�B
2!B
1AB
1B
0B
1B
0�B
/	B
-dB
-EB
-�B
-}B
-B
,�B
-�B
-iB
,�B
*�B
)�B
)lB
(bB
&[B
#�B
!�B
 �B
 rB
B
�B
tB
�B
�B
�B
�B
�B
B
WB
0B
!�B
 �B
!�B
#
B
#]B
#B
#�B
!QB
 �B
!1B
aB
`B
�B
�B
MB
B
qB
NB
B
jB
^B
bB
B
�B
B
B
�B

B
�B
sB
9B
�B
eB
GB
B
�B
�B
`B
RB
�B
MB
�B
�B
�B
�B
�B
�B
B
�B
�B
-B
�B
�B
hB
�B
xB
�B
�B
sB
<B
�B
&B
7B
�B
B
�B
�B
bB
EB
�B
�B
�B
KB
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
iB
�B
�B
�B
B
�B
�B
B
�B
$B
�B
B
�B
yB
B
(B
�B
�B
?B
gB
'B
�B
}B
�B
�B
�B
{B
>B
�B
�B
�B
rB
<B
B
�B
�B
aB
�B
GB
ZB
�B
�B
�B
B
�B
6B
:B
xB
�B
�B
�B
HB
�B
�B
CB
lB
�B
B
�B
 B
�B
�B
�B
zB
�B
sB
;B
�B
B
;B
8B
�B
�B
 9B
 �B
 �B
 �B
 �B
!rB
 �B
 �B
!~B
 �B
"�B
$,B
#&B
#fB
#�B
#�B
$^B
$B
$|B
%�B
&�B
&!B
&2B
&�B
'�B
(�B
(�B
(�B
)�B
)(B
)aB
)mB
)�B
*�B
*�B
+FB
+�B
+�B
+�B
+�B
,B
,�B
-xB
-�B
-{B
.�B
/YB
.�B
/:B
0B
0AB
0B
0qB
0�B
2KB
1�B
2~B
26B
2B
2�B
2qB
2�B
2ZB
2�B
2�B
2�B
3GB
3�B
3�B
4FB
4B
4B
4�B
5HB
5�B
5�B
67B
7iB
7�B
7�B
7B
7QB
8B
8kB
8�B
9B
9\B
:@B
:|B
9�B
:)B
:"B
:gB
;�B
;LB
;�B
<4B
<�B
<�B
<�B
=B
=xB
=oB
={B
> B
?B
? B
?�B
@<B
@�B
@yB
>qB
>�B
>�B
?�B
>�B
>�B
>�B
?hB
?wB
?�B
AB
@�B
@�B
A$B
A�B
B<B
A�B
AhB
BB
B�B
CVB
C�B
DB
DyB
D�B
D�B
F�B
G^B
G�B
HmB
I�B
J�B
KhB
K�B
J�B
H�B
I�B
ISB
J'B
I�B
J�B
KqB
J�B
KB
K7B
K�B
K�B
LGB
LVB
LyB
K�B
L�B
L�B
MbB
NB
NpB
M*B
N3B
OcB
NTB
N}B
N}B
O#B
OZB
PB
PB
P:B
P�B
P�B
PwB
PlB
QtB
P�B
Q}B
Q�B
Q�B
Q�B
RoB
R,B
Q�B
R�B
S�B
S}B
T B
S�B
S�B
S�B
T�B
T`B
T�B
UB
T�B
T�B
T�B
UB
U�B
V�B
U�B
VJB
ViB
V�B
VzB
V�B
V|B
W;B
W�B
V�B
WbB
WeB
WB
W�B
W�B
WoB
XB
Y�B
X�B
X�B
Z8B
Y�B
ZDB
Z�B
Z*B
Z�B
[�B
[�B
\zB
[OB
\	B
[�B
[�B
\�B
\WB
\�B
[�B
\�B
]jB
]xB
]qB
^QB
^ B
]�B
^HB
]�B
^�B
^TB
^fB
^�B
_%B
_|B
^�B
_�B
`�B
`B
_�B
_�B
aLB
`�B
`B
` B
`EB
`�B
a4B
avB
aYB
`�B
aB
a5B
`�B
`�B
`�B
b)B
b�B
c/B
c�B
c�B
dB
c;B
c�B
c�B
c�B
dfB
dB
dB
dB
dnB
d�B
e B
e!B
etB
e�B
e�B
e�B
f@B
fCB
fpB
gB
g1B
f�B
g#B
gB
g�B
g�B
g�B
g�B
hdB
hzB
h6B
h;B
iBB
i�B
i�B
i�B
i�B
i�B
j@B
j�B
k�B
kBB
k�B
l"B
l_B
l�B
l�B
m B
l�B
m3B
l�B
mB
m�B
n�B
n�B
n�B
o�B
peB
o�B
pB
pB
o�B
p>B
o�B
p9B
pPB
p1B
pYB
q<B
q�B
q�B
r�B
rmB
rkB
r_B
r�B
r�B
s+B
slB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t>B
t�B
t8B
t~B
uB
uXB
t�B
t�B
t�B
u�B
u�B
v#B
v^B
vB
v^B
u�B
vmB
v�B
wB
v�B
wPB
w�B
w�B
w�B
w�B
x#B
xB
w�B
w[B
wwB
w�B
x^B
x�B
x�B
x�B
w�B
xVB
x�B
yqB
y;B
y�B
zsB
zB
zB
z#B
zIB
zYB
z�B
{(B
z�B
{B
{�B
{vB
{B
{`B
{�B
{�B
|�B
|�B
|eB
|fB
|sB
|�B
|�B
}B
|�B
|�B
}B
}	B
} B
}�B
}�B
}{B
}�B
}kB
}�B
}BB
~	B
}�B
~=B
~�B
~�B
~8B
~vB
B
pB
�B
�B
B
B
�B
�B
�B
�B
 B
gB
BB
jB
~�B
�B
��B
�B
�hB
�HB
�9B
�B
��B
�\B
�XB
�B
��B
�*B
�{B
�-B
��B
��B
��B
��B
��B
��B
��B
��B
� B
�;B
��B
�B
�B
��B
��B
��B
��B
�>B
��B
�	B
�B
�3B
��B
�B
�NB
�B
��B
��B
��B
��B
��B
��B
�;B
�B
��B
��B
��B
��B
�
B
�B
�B
��B
�`B
��B
�B
��B
�ZB
��B
��B
�dG�O�B��B��B��B��B�B�FB��B�B�zB��B�B�nB�zB��B�?B�FB��B�B�?B�zB��B�tB��B��B�zB��B�LB�B��B��B�RB��B�tB��B��B�9B��B�B�B��B�tB��B��B�zB��B��B��B�B��B��B�B�LB��B��B��B�LB��B�tB�zB�B��B��B��B��B��B�B��B��B�zB�zB��B��B��B��B��B��B�FB�tB��B�B�FB�tB�FB��B��B�zB��B��B��B�zB�FB�LB��B�LB�zB�LB��B�B�B�LB��B��B��B��B��B��B�B�B�RB��B�FB�zB��B�RB�LB��B�LB��B�zB�B�FB�LB��B��B��B�LB��B�B�zB��B��B��B�tB��B�B��B�zB��B�LB��B�B��B��B�LB�B��B�B��B�FB�B�zB��B�FB��B�FB�LB��B��B��B�LB�zB��B�B�B��B��B��B��B�tB�nB�zB�FB��B�FB�B�FB�B��B�tB��B��B�?B�B�B�LB��B��B��B�B�B��B�?B��B��B�LB��B�B�tB�B�LB�FB�B��B�B�zB�?B��B�zB��B�FB�B�B��B��B��B��B��B��B�tB�FB�B�nB��B�tB��B��B�9B�B��B��B�B�LB�?B��B��B��B��B�nB��B�'B�EBǮB�tB��BʌB��B�NB�BΥB�B�mBںB֡BیB��B�BچB�B��B��B�yB	 �B�JB	B	S�B	�OB	��B	��B	��B	��B	�)B	��B
�B
n�B
ZQB
[�B
[�B
[#B
\)B
]dB
^�B
^�B
^�B
_B
_pB
_�B
_pB
^B
\�B
\)B
[�B
\�B
]�B
_pB
_�B
aB
f�B
a�B
bB
f2B
l�B
u�B
x8B
zxB
}VB
x�B
��B
zB
�GB
�DB
��B
�oB
��B
��B
��B
��B
��B
��B
��B
��B
�kB
��B
��B
��B
�\B
��B
��B
�VB
�(B
�@B
� B
�B
��B
� B
��B
�B
��B
��B
�oB
�B
��B
�bB
��B
�.B
� B
�.B
�SB
�@B
��B
��B
��B
��B
��B
��B
�4B
��B
�$B
ǮB
�EB
�B
��B
�B
��B
�B
�B
�NB
�B
�ZB
��B
� B
�TB
�B
�B
�B
��B
�B
�ZB
�&B
�ZB
�ZB
�B
�|B
��B
�B
�B
�pB
�B
ޞB
��B
�;B
�5B
�pB
�B
��B
��B
�]B
�pB
�vB
�dB
�B
�B
ٴB
ӏB
�TB
ӏB
�)B
�B
��B
�B
��B
��B
ѷB
�B
�mB
��B
�B
�tB
��B
�tB
��B
�B
��B
ߤB
	B
��B
�?B
�B
��B
��B
��B
�qB
�qB
��B
��B
�*B
��B
��B
��B
�OB
�B
��B
��B
��B
��B
�FB
��B
�1B
�VB
�1B
�MB
��B
�4B
}�B
.B
�B
}"B
|PB
z�B
u�B
s�B
s�B
qAB
poB
sMB
u%B
n�B
l�B
l�B
n�B
k�B
n�B
n/B
n/B
ffB
dZB
d�B
cTB
b�B
aHB
a|B
c B
]dB
d�B
NpB
d&B
Z�B
b�B
_�B
YB
U�B
Q�B
hsB
j�B
[�B
e�B
QB
5tB
/�B
OB
T�B
VmB
K)B
B�B
5�B
0UB
1'B
0UB
33B
��B
^�B
:*B
4nB
%�B
!�B
C�B
gmB
%�B
!-B
�B
FB
�B
B
�B
PB
xB

�B

=B
xB

	B
~B
	B
1B
+B
B
�B
MB
�B
B
_B	�.B	�VB
SB	��B
MB
1B	��B	��B
B
GB	��B	��B	�(B	�.B	��B	�VB	��B	��B	��B	�B	�xB	��B	�B	�PB	�B	��B	��B	��B	��B	��B
�B
�B
{B
B
oB
�B
B
B
 iB
 iB	��B
�B	��B
�B
�B
bB
�B
�B	�B	�B	��B	�B
�B	�B	�DB	�B	��B	��B	� B	�iB	�B	��B	�)B	�B	ٴB	�?B	�QB	خB	�#B	�yB	��B	��B	уB	�}B	бB	ҽB	��B	��B	�}B	͟B	�)B	��B	ɺB	̘B	��B	�B	��B	�<B	ȴB	�mB	�#B	��B	�9B	�B	�aB	�	B	�TB	��B	�-B	��B	��B	�zB	��B	��B	�zB	��B	�=B	�kB	��B	��B	��B	��B	��B	�9B	�B	��B	�bB	��B	�$B	��B	��B	�B	��B	��B	~�B	��B	��B	yrB	k�B	o B	kQB	d�B	b�B	c�B	dZB	i�B	d�B	d�B	aHB	b�B	b�B	a|B	b�B	`�B	e`B	`�B	\]B	Z�B	aHB	Z�B	e�B	XyB	WsB	h�B	V9B	N�B	E�B	@�B	<jB	7B	CaB	R B	UgB	m�B	9XB	&�B	$tB	�B	"�B	,B	�B	�B	FB	%B	�B	4B	�B	oB	uB	�B	uB	�B	B	�B	eB	�B	xB	B	OB	~B	!B	�B	 �B	 �B	#�B	$tB	#nB	*�B	*eB	-B	.B	0!B	1[B	0!B	0UB	3hB	/�B	2�B	5�B	7LB	?�B	L�B	R�B	T�B	UgB	V�B	u%B	w2B	xlB	|�B	}"B	}VB	~]B	�iB	�uB	��B	�B	��B	�B	�uB	��B	��B	��B	�B	��B	��B	B	�B	�$B	��B	�XB	��B	��B	��B	�*B	�B	�}B	�qB	�wB	�BB	�HB	��B	��B	��B	��B	�mB	��B	��B	�mB	�zB	��B	�9B	�-B	��B	�[B	��B	�pB	��B	уB	бB	҉B	�aB	֡B	�9B	�B	՛B	�,B	�gB	�9B	�}B	��B	��B	�B	�`B	��B	��B	��B	��B	��B	�|B	�(B
�B	�B	��B	�+B	��B	�B	�WB
�B
\B
{B
�B
=B
qB
%zB
�B
+6B
49B
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
=G='J�<K?�= '<��<��Y<�qW<#�
<C��<�qc<���<#�
<`��<*�~<1�o=�T=1h�={<\�<#�
<'��<#�
<#�
<#�
<#�
<��<�<�<Of�<#�
<#�
<#�
<��_<��G<o/�<�p�<�F<#�
<#�
<G��<�� <��T<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2020021709571520200217095715IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020022710005120200227100051QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020022710005120200227100051QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020082411445120200824114451IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                