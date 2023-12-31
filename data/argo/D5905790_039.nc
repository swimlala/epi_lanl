CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  	   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-03-19T11:49:23Z creation; 2021-02-12T22:10:01Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     H  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   UP   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     H  [d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   s�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     H  y�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     H  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     H  �d   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Ȭ   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     H  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     H  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     H d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     H #�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` <   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   <h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   Bh   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   Hh   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T Nh   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   N�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   N�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   N�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   N�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � N�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   O\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   Ox   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    O�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        O�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        O�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       O�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    O�Argo profile    3.1 1.2 19500101000000  20200319114923  20210212221001  5905790 5905790 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               '   'AA  AOAO7824_008764_039                 7824_008764_039                 2C  2C  DD  SOLO_II                         SOLO_II                         8764                            8764                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @�[��gw@�[��gw11  @�[��r@�[��r@7*K3���@7*K3����d��pO�4�d��pO�411  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @@  @�  @��R@�  @�G�A ��A��A   A+�A@  A`��A\)A��A��A�\)A�\)A�  A�Q�A�Q�B   B�
B(�B(�B   B'�
B/�
B8(�B@(�BH(�BP  BW�
B_�
Bg�
Bp  Bx(�B�  B��B�  B�{B�  B�  B�{B�  B��B�  B�{B�{B�  B��B��B��B��B�  B�  B�  B�{B�  B��B�  B�  B��B�  B��B��B�  B��B��B��C��C  C  C  C
  C  C  C��C  C  C��C  C{C  C��C��C"  C$
=C&
=C(
=C*
=C,
=C.  C0
=C2
=C3��C5�C7�C9��C<  C=��C?��CA�CC��CF  CH  CJ  CL  CN  CP
=CR
=CS��CV  CX  CY��C[�C]��C`  Ca��Cc��Cf
=Ch
=Cj
=Cl
=Cn  Cp  Cr  Ct  Cv{Cx
=Cz  C|  C~  C�  C���C�  C���C���C�  C�  C�  C�  C�  C�  C���C���C�C�
=C�C�C�C�  C�  C�  C���C���C�  C�C���C���C���C���C���C�  C���C���C�  C�  C�C�C�C�C�C�C�C�  C���C���C���C�  C�C�C�C�C�
=C�  C���C�
=C�  C�  C�C�  C�  C���C�  C�  C�  C�C�C�  C�  C�C�C�C�  C�  C�C�  C�C�  C�  C�  C���C���C�  C�  C�  C���C�  C�  C���C�  C���C�  C�C���C���C�C�C�  C�  C�  C�  C�  C�  C���C�C�C�  C�C���C���C���C���C���C�  C�C�  C���C���C�C�C�  C���C���C�C�C�  C�C�
=C�D �D ��D  D� D�D� D  D� D�qD� D�D� D  D� D  D��D�qD}qD�qD	� D
  D
� D  D� D�D� D�qD}qD�qD}qD�qD� D�qD}qD�qD}qD  D��D  D� D  D}qD�qD� D  D� D  D� D�D� D�qD��DD� D  D� D  D� D  D��D  Dz�D�qD}qD   D � D!�D!��D"  D"}qD#  D#� D$  D$��D%D%�D&  D&� D'D'�D(  D(z�D(�qD)� D*�D*��D+�D+� D+�qD,� D-�D-� D-�qD.��D/  D/}qD/�qD0}qD0�qD1� D2  D2� D2�qD3� D4  D4}qD5  D5��D6  D6� D7�D7��D8  D8��D9�D9� D:�D:��D;�D;��D<  D<� D=  D=� D>  D>� D>��D?}qD@  D@� DA  DA� DB�DB� DC  DC� DD  DD� DD�qDEz�DF  DF�DG  DG� DG�qDH}qDH�qDI}qDJ  DJ� DK  DK}qDL  DL� DL�qDM� DN�DN��DO  DO� DP  DP� DP�qDQ}qDR�DR��DS�DS��DT  DT� DU�DU� DV  DV��DW  DW� DX  DX� DY  DY��DZ  DZ��D[�D[}qD\  D\� D]  D]��D^  D^��D_  D_}qD`  D`� D`�qDa� Da�qDb}qDc  Dc��DdDd��Dd��De}qDe�qDf}qDf�qDg� Dh�Dh^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��?#�
?W
=?�=q?��
?��?�G�@�@z�@!G�@0��@G�@Tz�@c�
@u@��
@���@�z�@�p�@��@��@���@�G�@˅@�z�@�p�@��@�{@���A   Az�A��A{A33A
=A�A   A%�A(��A.{A2�\A7
=A:�HA?\)AC�
AHQ�AL��AQ�AVffAZ�HA`  Ae�Ai��An�RAs�
AxQ�A~{A��A�z�A�
=A���A�(�A��RA���A�(�A�
=A��A���A��A��\A�p�A�  A�33A�{A�G�A�(�A��RA��A�z�A�
=A�=qA��A�  A�33A�AУ�AӅA�ffA�G�A�z�A�
=A��A�z�A�\)A��A���A�A�\A��A��A��\A���A�\)B ��B=qB\)B��B{B33B��B	�B
=BQ�B��B�HB  Bp�B�\B�B�B=qB�B��B�B
=BQ�Bp�B�RB   B!�B"ffB#�B$��B&{B'33B(z�B)��B+
=B,(�B-G�B.ffB/�B0��B1�B3
=B4Q�B5��B6�RB8  B9G�B:�RB<  B=G�B>ffB?�
BA�BB�\BC�BD��BF{BG�BH��BJ{BK33BL��BMBO
=BP(�BQ��BR�RBT  BUp�BV�\BW�
BY�BZ=qB[�B\��B]�B_33B`Q�BaBc
=Bd  BeG�BfffBg�Bh��Bi�Bk33BlQ�Bmp�Bn�\Bo�Bp��Bq�Br�HBt(�Bt��Bu�Bv�RBw�Bx��ByG�Bz{Bz�RB{�B|Q�B}�B}B~=qB\)B�  B�z�B���B��B��B��
B�Q�B��RB�
=B��B��
B�Q�B��\B�
=B�\)B�B�=qB��\B���B�G�B�B�(�B��\B���B�\)B�B�(�B��\B�
=B�p�B��
B�Q�B��RB��B��B��B�Q�B���B��B��B��B�Q�B���B�
=B�p�B��
B�=qB���B���B�p�B�B�=qB���B�
=B�\)B�B�(�B���B���B�\)B�B�{B�z�B��HB�33B��B�  B�ffB���B�33B��B��
B�(�B��\B���B�G�B��B�{B�ffB���B�33B���B��
B�Q�B��RB��B�\)B��
B�=qB���B���B�\)B��B�(�B�z�B��HB�G�B���B�  B�ffB��RB��B��B��B�=qB���B���B�\)B�B�(�B��\B��HB�\)B��B�{B�z�B���B�33B��B�  B�Q�B��RB��B�p�B��
B�=qB���B�
=B�p�B�B�=qB��\B���B�\)B�B�(�B�ffB��HB�G�B��B�{B�ffB��HB�G�B��B�  B�ffB��HB�33B�B�{B�z�B��HB�\)B�B�(�B��\B�
=B�\)B��
B�=qB���B�
=B�\)B��
B�=qB���B�
=B�p�B��
B�(�B£�B���B�p�B�B�=qBď\B��HB�\)B�B�{BƏ\B��HB�\)B�B�(�Bȣ�B���B�p�B��
B�=qBʣ�B��B˅B��B�ffB���B�33B͙�B�  B�ffB���B�33Bϙ�B�{B�z�B��HB�\)BѮB�{Bҏ\B��HB�\)B�B�=qBԏ\B�
=B�\)B��
B�=qB֏\B�
=B�\)B�B�=qB؏\B�
=B�p�B��
B�=qBڸRB��BۅB��B�Q�BܸRB��B݅B��B�ffB��HB�33Bߙ�B�  B�ffB���B�G�BᙚB�  B�ffB��HB�G�B�B�(�B�\B�
=B�B��B�Q�B���B�G�B癚B�{B�\B�
=B�p�B��
B�Q�B�RB��B뙚B�  B�z�B���B�G�B�B�(�B��B�
=BB�  B��\B�
=B�p�B��
B�z�B��HB�p�B��
B�Q�B��HB�\)B��
B�=qB���B�G�B��
B�Q�B���B�G�B�B�=qB���B�G�B�B�Q�B��RB�\)B��
B�Q�B���B�G�B�C (�C ffC ��C �C(�CffC�C�C(�CffC�C�C33Cp�C��C�C33Cp�C�C�C33Cp�C�C�C(�Cp�C�C��C33CffC�C�C33Cp�C�C�C	=qC	p�C	C	��C
=qC
z�C
C
=CQ�C��C�
C�Cp�C��C��C=qC�C��C
=CQ�C��C�HC=qCp�CC
=CQ�C�\C�HC(�CffC�RC  CG�C�\C��C{C\)C�C�C33C�C��C{CQ�C��C�C(�Cp�CC
=CQ�C��C�HC(�Cp�C�RC  CG�C�\C�
C(�Cp�C�C  CG�C�\C�
C�Cp�C�RC  CQ�C��C�HC(�Cp�CC
=CQ�C�\C�HC (�C p�C �RC!
=C!Q�C!��C!�C"33C"�C"��C#{C#\)C#�C#��C$G�C$�\C$�
C%�C%ffC%�RC&  C&G�C&�\C&�
C'�C'p�C'�C'��C(=qC(�\C(�
C)�C)\)C)��C)��C*G�C*�C*��C+�C+\)C+�C+�C,=qC,�C,�
C-�C-p�C-�C.  C.Q�C.��C.�C/33C/z�C/�
C0{C0ffC0�C1  C1=qC1��C1�HC2(�C2z�C2��C3{C3ffC3�C4  C4G�C4�\C4�HC533C5z�C5��C6{C6ffC6�C7  C7Q�C7��C7��C8=qC8�\C8�
C9(�C9p�C9C:
=C:\)C:��C:��C;G�C;�\C;�
C<(�C<z�C<C=
=C=\)C=�C=��C>G�C>�\C>�C?(�C?�C?��C@�C@p�C@�RCA  CAQ�CA��CA��CB=qCB�\CB�HCC33CCz�CCCD{CDffCD�RCE
=CEG�CE��CE�CF=qCF�CF�
CG�CGp�CG��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?��@   @@  @�  @��R@�  @�G�A ��A��A   A+�A@  A`��A\)A��A��A�\)A�\)A�  A�Q�A�Q�B   B�
B(�B(�B   B'�
B/�
B8(�B@(�BH(�BP  BW�
B_�
Bg�
Bp  Bx(�B�  B��B�  B�{B�  B�  B�{B�  B��B�  B�{B�{B�  B��B��B��B��B�  B�  B�  B�{B�  B��B�  B�  B��B�  B��B��B�  B��B��B��C��C  C  C  C
  C  C  C��C  C  C��C  C{C  C��C��C"  C$
=C&
=C(
=C*
=C,
=C.  C0
=C2
=C3��C5�C7�C9��C<  C=��C?��CA�CC��CF  CH  CJ  CL  CN  CP
=CR
=CS��CV  CX  CY��C[�C]��C`  Ca��Cc��Cf
=Ch
=Cj
=Cl
=Cn  Cp  Cr  Ct  Cv{Cx
=Cz  C|  C~  C�  C���C�  C���C���C�  C�  C�  C�  C�  C�  C���C���C�C�
=C�C�C�C�  C�  C�  C���C���C�  C�C���C���C���C���C���C�  C���C���C�  C�  C�C�C�C�C�C�C�C�  C���C���C���C�  C�C�C�C�C�
=C�  C���C�
=C�  C�  C�C�  C�  C���C�  C�  C�  C�C�C�  C�  C�C�C�C�  C�  C�C�  C�C�  C�  C�  C���C���C�  C�  C�  C���C�  C�  C���C�  C���C�  C�C���C���C�C�C�  C�  C�  C�  C�  C�  C���C�C�C�  C�C���C���C���C���C���C�  C�C�  C���C���C�C�C�  C���C���C�C�C�  C�C�
=C�D �D ��D  D� D�D� D  D� D�qD� D�D� D  D� D  D��D�qD}qD�qD	� D
  D
� D  D� D�D� D�qD}qD�qD}qD�qD� D�qD}qD�qD}qD  D��D  D� D  D}qD�qD� D  D� D  D� D�D� D�qD��DD� D  D� D  D� D  D��D  Dz�D�qD}qD   D � D!�D!��D"  D"}qD#  D#� D$  D$��D%D%�D&  D&� D'D'�D(  D(z�D(�qD)� D*�D*��D+�D+� D+�qD,� D-�D-� D-�qD.��D/  D/}qD/�qD0}qD0�qD1� D2  D2� D2�qD3� D4  D4}qD5  D5��D6  D6� D7�D7��D8  D8��D9�D9� D:�D:��D;�D;��D<  D<� D=  D=� D>  D>� D>��D?}qD@  D@� DA  DA� DB�DB� DC  DC� DD  DD� DD�qDEz�DF  DF�DG  DG� DG�qDH}qDH�qDI}qDJ  DJ� DK  DK}qDL  DL� DL�qDM� DN�DN��DO  DO� DP  DP� DP�qDQ}qDR�DR��DS�DS��DT  DT� DU�DU� DV  DV��DW  DW� DX  DX� DY  DY��DZ  DZ��D[�D[}qD\  D\� D]  D]��D^  D^��D_  D_}qD`  D`� D`�qDa� Da�qDb}qDc  Dc��DdDd��Dd��De}qDe�qDf}qDf�qDg� Dh�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��?#�
?W
=?�=q?��
?��?�G�@�@z�@!G�@0��@G�@Tz�@c�
@u@��
@���@�z�@�p�@��@��@���@�G�@˅@�z�@�p�@��@�{@���A   Az�A��A{A33A
=A�A   A%�A(��A.{A2�\A7
=A:�HA?\)AC�
AHQ�AL��AQ�AVffAZ�HA`  Ae�Ai��An�RAs�
AxQ�A~{A��A�z�A�
=A���A�(�A��RA���A�(�A�
=A��A���A��A��\A�p�A�  A�33A�{A�G�A�(�A��RA��A�z�A�
=A�=qA��A�  A�33A�AУ�AӅA�ffA�G�A�z�A�
=A��A�z�A�\)A��A���A�A�\A��A��A��\A���A�\)B ��B=qB\)B��B{B33B��B	�B
=BQ�B��B�HB  Bp�B�\B�B�B=qB�B��B�B
=BQ�Bp�B�RB   B!�B"ffB#�B$��B&{B'33B(z�B)��B+
=B,(�B-G�B.ffB/�B0��B1�B3
=B4Q�B5��B6�RB8  B9G�B:�RB<  B=G�B>ffB?�
BA�BB�\BC�BD��BF{BG�BH��BJ{BK33BL��BMBO
=BP(�BQ��BR�RBT  BUp�BV�\BW�
BY�BZ=qB[�B\��B]�B_33B`Q�BaBc
=Bd  BeG�BfffBg�Bh��Bi�Bk33BlQ�Bmp�Bn�\Bo�Bp��Bq�Br�HBt(�Bt��Bu�Bv�RBw�Bx��ByG�Bz{Bz�RB{�B|Q�B}�B}B~=qB\)B�  B�z�B���B��B��B��
B�Q�B��RB�
=B��B��
B�Q�B��\B�
=B�\)B�B�=qB��\B���B�G�B�B�(�B��\B���B�\)B�B�(�B��\B�
=B�p�B��
B�Q�B��RB��B��B��B�Q�B���B��B��B��B�Q�B���B�
=B�p�B��
B�=qB���B���B�p�B�B�=qB���B�
=B�\)B�B�(�B���B���B�\)B�B�{B�z�B��HB�33B��B�  B�ffB���B�33B��B��
B�(�B��\B���B�G�B��B�{B�ffB���B�33B���B��
B�Q�B��RB��B�\)B��
B�=qB���B���B�\)B��B�(�B�z�B��HB�G�B���B�  B�ffB��RB��B��B��B�=qB���B���B�\)B�B�(�B��\B��HB�\)B��B�{B�z�B���B�33B��B�  B�Q�B��RB��B�p�B��
B�=qB���B�
=B�p�B�B�=qB��\B���B�\)B�B�(�B�ffB��HB�G�B��B�{B�ffB��HB�G�B��B�  B�ffB��HB�33B�B�{B�z�B��HB�\)B�B�(�B��\B�
=B�\)B��
B�=qB���B�
=B�\)B��
B�=qB���B�
=B�p�B��
B�(�B£�B���B�p�B�B�=qBď\B��HB�\)B�B�{BƏ\B��HB�\)B�B�(�Bȣ�B���B�p�B��
B�=qBʣ�B��B˅B��B�ffB���B�33B͙�B�  B�ffB���B�33Bϙ�B�{B�z�B��HB�\)BѮB�{Bҏ\B��HB�\)B�B�=qBԏ\B�
=B�\)B��
B�=qB֏\B�
=B�\)B�B�=qB؏\B�
=B�p�B��
B�=qBڸRB��BۅB��B�Q�BܸRB��B݅B��B�ffB��HB�33Bߙ�B�  B�ffB���B�G�BᙚB�  B�ffB��HB�G�B�B�(�B�\B�
=B�B��B�Q�B���B�G�B癚B�{B�\B�
=B�p�B��
B�Q�B�RB��B뙚B�  B�z�B���B�G�B�B�(�B��B�
=BB�  B��\B�
=B�p�B��
B�z�B��HB�p�B��
B�Q�B��HB�\)B��
B�=qB���B�G�B��
B�Q�B���B�G�B�B�=qB���B�G�B�B�Q�B��RB�\)B��
B�Q�B���B�G�B�C (�C ffC ��C �C(�CffC�C�C(�CffC�C�C33Cp�C��C�C33Cp�C�C�C33Cp�C�C�C(�Cp�C�C��C33CffC�C�C33Cp�C�C�C	=qC	p�C	C	��C
=qC
z�C
C
=CQ�C��C�
C�Cp�C��C��C=qC�C��C
=CQ�C��C�HC=qCp�CC
=CQ�C�\C�HC(�CffC�RC  CG�C�\C��C{C\)C�C�C33C�C��C{CQ�C��C�C(�Cp�CC
=CQ�C��C�HC(�Cp�C�RC  CG�C�\C�
C(�Cp�C�C  CG�C�\C�
C�Cp�C�RC  CQ�C��C�HC(�Cp�CC
=CQ�C�\C�HC (�C p�C �RC!
=C!Q�C!��C!�C"33C"�C"��C#{C#\)C#�C#��C$G�C$�\C$�
C%�C%ffC%�RC&  C&G�C&�\C&�
C'�C'p�C'�C'��C(=qC(�\C(�
C)�C)\)C)��C)��C*G�C*�C*��C+�C+\)C+�C+�C,=qC,�C,�
C-�C-p�C-�C.  C.Q�C.��C.�C/33C/z�C/�
C0{C0ffC0�C1  C1=qC1��C1�HC2(�C2z�C2��C3{C3ffC3�C4  C4G�C4�\C4�HC533C5z�C5��C6{C6ffC6�C7  C7Q�C7��C7��C8=qC8�\C8�
C9(�C9p�C9C:
=C:\)C:��C:��C;G�C;�\C;�
C<(�C<z�C<C=
=C=\)C=�C=��C>G�C>�\C>�C?(�C?�C?��C@�C@p�C@�RCA  CAQ�CA��CA��CB=qCB�\CB�HCC33CCz�CCCD{CDffCD�RCE
=CEG�CE��CE�CF=qCF�CF�
CG�CGp�CG��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A��;A��A��A��A��A��A���A���A���A���A���A���A���A�  A�  A�A�%A�1A�1A�1A�
=A�
=A�JA�
=A�VA�%A��yA��RA��DA�9XA��A�oA���A��mA��jA�A�A��A��uA���A�v�A���A�M�A�E�A�A�ffA���A��uA��;A�M�A�1A��FA�XA���A�jA��#A���A���A���A�Q�A���A�=qA�t�A�S�A��A�33A�&�A�7LA���A�-A��
A��RA��yA���A���A��FA��A�  A�ĜA��^A�\)A�A�A��mA��hA�=qA�
=A��A�\)A��FA�|�A���A��A�VA���A��A��yA��A��PA�x�A�O�A���A��mA�~�A��wA�K�A�  A�r�A��^A�?}A�%A�"�A�7LA���A��A�jA��#A�=qA�5?A~^5A|��A{�#A{33Azn�Ay��Ayx�AwO�Av�jAv�uAvJAu�-AuXAt1Ap$�An�yAm|�Al �Af  A`E�A^M�A[��AZ�uAY%AVQ�AT�AQ��AO�AKƨAJ=qAI�
AH��AG�AE�TAE`BAEVADZAD5?AD1AC�FAC�ACdZAC7LAC33AB��A?��A=;dA<��A;�A9��A8ZA7XA6bNA4v�A3l�A3&�A2�HA2=qA17LA/��A-�A-/A,�9A,bA+�PA*�yA)��A(E�A'K�A$��A#hsA#O�A#7LA#�A"�`A"ffA!�A!K�A E�A�uA��AdZAt�A�A=qA�mA�PAoA�A1'A1Ax�A5?A�FA(�A�jA�Av�A(�A��AS�A�`A�uA �A��AO�A$�A
�uA	��A�/AffA��A�A�#AK�A�DA�A�`A^5A
=A��AJA��A��Al�A �@�%@���@���@�@���@�o@�{@���@�+@�\)@�~�@�@��H@�$�@�`B@��@���@�ȴ@�J@�Z@�t�@ް!@�/@��y@�p�@�Z@�n�@�O�@ԃ@�|�@�
=@��@���@�&�@ϕ�@��@Η�@̛�@�b@˥�@��@���@�+@�
=@���@���@ư!@Ƨ�@Ƨ�@Ə\@�v�@�V@�5?@őh@Ĭ@�r�@Ý�@���@�ƨ@�33@�=q@�?}@�X@�I�@�\)@��H@��!@��T@���@��@��@�n�@�=q@��@�@��h@�%@�1'@��m@��
@��w@��P@�|�@�t�@�dZ@�"�@���@���@��m@�;d@��@�ȴ@���@��\@�v�@�^5@�E�@��-@�&�@�  @��w@��H@�~�@�V@��`@��@��w@�l�@�ff@�5?@�{@��^@��h@�O�@�7L@�&�@���@�I�@�C�@��@��@���@�ȴ@��!@���@���@�n�@�^5@�=q@�-@���@���@��^@��@�p�@�?}@��@�%@��@�I�@�A�@�I�@�b@��F@��@��@���@���@�l�@���@���@�n�@�{@���@��h@��/@���@�\)@�@��@��@��y@��!@�~�@�E�@�$�@��T@���@�&�@���@���@�9X@� �@� �@� �@�(�@�b@�dZ@��R@�$�@��/@�t�@�ȴ@��#@�G�@��@�%@��@���@��D@�r�@�Q�@�9X@�  @���@�C�@��@���@���@���@���@���@���@�v�@�ff@�=q@�$�@��@��@��-@�G�@�r�@�|�@�33@��@���@��R@��+@�5?@�{@��@���@��9@�~�@��#@���@���@��uG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��#A��/A��/A��;A��yA��A��yA��mA��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A���A���A�  A�A�A�A�A�A�A�A�A�A�A�A�A�%A�%A�%A�%A�%A�1A�1A�
=A�
=A�1A�1A�1A�1A�1A�1A�1A�1A�1A�1A�%A�%A�%A�1A�JA�
=A�JA�JA�JA�JA�
=A�1A�JA�VA�JA�VA�1A�1A�1A�A�%A�%A�
=A�
=A�
=A�JA�VA�JA�JA�1A�
=A�JA�JA�JA�%A�  A���A�  A�A���A��A��A��mA��/A��
A���A�A��^A��RA��FA��FA��9A��9A��-A���A��+A�v�A�jA�l�A�l�A�S�A�+A� �A��A��A��A��A��A��A��A��A��A�{A�{A�{A�oA�VA�A�A�  A���A���A��A��A��yA��yA��mA��yA��mA��TA��A���A���A��RA��9A��-A��A���A���A���A�t�A�;dA���A���A�n�A�;dA�+A�"�A��A�ȴA�ZA�;dA�  A��A��`A���A���A���A��-A��PA��A�n�A�`BA�S�A�9XA�5?A�bA�A��A��A��A��mA��`A��mA��HA��FA���A���A���A��hA�jA�\)A�M�A�;dA�"�A���A��mA�A��-A���A���A�x�A�r�A�l�A�ffA�^5A�ZA�O�A�O�A�K�A�I�A�I�A�G�A�E�A�E�A�E�A�C�A�C�A�G�A�E�A�G�A�G�A�E�A�G�A�E�A�A�A�7LA�1'A�/A�(�A��A��A�%A��^A���A��hA��\A��PA��A�r�A�dZA�^5A�Q�A�O�A�I�A�C�A�=qA�1'A�-A�&�A��A��#A���A���A�r�A�E�A�-A�  A���A���A��yA��RA��7A�`BA�=qA�+A��A�bA�JA�JA�1A�  A��A��yA��HA���A���A��A�p�A�hsA�^5A�S�A�K�A�G�A�E�A�A�A�?}A�=qA�7LA�33A�/A�-A�+A�"�A�  A��A���A�ƨA���A�ĜA��jA�A��RA��9A��!A��!A��!A��-A��-A��A���A��DA��A�bNA�XA�E�A�5?A��A�  A���A��`A��/A���A���A���A�ȴA��wA��^A��FA��-A���A��uA��A�t�A�M�A�;dA�5?A�33A�&�A� �A��A�VA�A���A��A��wA��-A��hA�r�A�K�A�$�A�A���A���A���A���A��PA�x�A�ffA�Q�A�C�A� �A���A��`A��#A���A���A�A��!A���A���A���A���A���A��uA��uA���A���A���A��hA��+A�l�A�Q�A�A�A�1'A�"�A��A�oA�
=A���A��TA��
A�A��\A�jA�\)A�M�A�1'A��A��A�|�A�;dA�
=A��mA��;A��#A���A���A���A��uA��PA�v�A�jA�hsA�`BA�\)A�S�A�I�A�"�A�A��mA��FA�33A�A��A�?}A��mA��wA���A�`BA�=qA��A��;A���A��7A�n�A�XA�Q�A�I�A�7LA�/A�+A�"�A��A�A�  A��TA��RA�p�A�&�A��TA���A�/A��A��A���A��A�`BA�$�A��A�n�A�VA�O�A�G�A�;dA��A��/A��!A���A���A���A�x�A�;dA�bA�A���A��A��A��`A��/A��#A��#A��
A���A�ƨA���A���A��jA��RA��9A��9A��FA��FA��-A���A�l�A�VA��TA���A���A�S�A�-A� �A���A�ȴA�~�A�bNA�`BA�S�A�?}A�&�A�
=A�VA��A�ĜA��+A�ffA�33A�
=A��/A���A��9A��+A��A��A�v�A�hsA�M�A�-A��A��!A�`BA�$�A�oA�oA�VA�
=A���A��#A���A���A���A�ĜA���A��wA��wA��jA��jA��^A��^A��jA��jA��^A��RA��FA���A���A��A�S�A�5?A�ȴA�hsA�O�A�G�A�;dA�9XA�33A�+A��A�A��A��A��/A��
A���A��wA��^A��A���A��\A�z�A�r�A�hsA�XA�G�A�?}A�9XA�-A��A�{A��mA�I�A��TA��!A��+A�ffA�?}A�1A���A��DA�O�A��A�A��yA���A���A�jA�+A���A��mA��A���A�ĜA��FA���A���A���A���A���A��A�v�A�r�A�p�A�p�A�jA�&�A���A���A�~�A�S�A��A�ƨA���A��uA�\)A��!A�(�A���A��hA�=qA���A�{A��A�ƨA��!A��A�A�A��;A���A��A�v�A�9XA�+A�&�A��A��A��A�bA�A���A���A��A��yA��;A���A���A�A��^A���A���A���A���A��uA��hA��hA��PA��DA��+A��A��A��A�r�A�x�A�n�A�`BA�\)A�ZA�S�A�S�A�Q�A�A�A�7LA�=qA�+A��A���A���A��!A��A�A��yA��HA��#A���A�ƨA��-A���A��hA�|�A�ffA�5?A�$�A�
=A��mA��-A��hA�~�A�ffA�^5A�XA�K�A�E�A�=qA�;dA�5?A�(�A�$�A�A��A��;A���A��-A���A��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��;A��A��A��A��A��A���A���A���A���A���A���A���A�  A�  A�A�%A�1A�1A�1A�
=A�
=A�JA�
=A�VA�%A��yA��RA��DA�9XA��A�oA���A��mA��jA�A�A��A��uA���A�v�A���A�M�A�E�A�A�ffA���A��uA��;A�M�A�1A��FA�XA���A�jA��#A���A���A���A�Q�A���A�=qA�t�A�S�A��A�33A�&�A�7LA���A�-A��
A��RA��yA���A���A��FA��A�  A�ĜA��^A�\)A�A�A��mA��hA�=qA�
=A��A�\)A��FA�|�A���A��A�VA���A��A��yA��A��PA�x�A�O�A���A��mA�~�A��wA�K�A�  A�r�A��^A�?}A�%A�"�A�7LA���A��A�jA��#A�=qA�5?A~^5A|��A{�#A{33Azn�Ay��Ayx�AwO�Av�jAv�uAvJAu�-AuXAt1Ap$�An�yAm|�Al �Af  A`E�A^M�A[��AZ�uAY%AVQ�AT�AQ��AO�AKƨAJ=qAI�
AH��AG�AE�TAE`BAEVADZAD5?AD1AC�FAC�ACdZAC7LAC33AB��A?��A=;dA<��A;�A9��A8ZA7XA6bNA4v�A3l�A3&�A2�HA2=qA17LA/��A-�A-/A,�9A,bA+�PA*�yA)��A(E�A'K�A$��A#hsA#O�A#7LA#�A"�`A"ffA!�A!K�A E�A�uA��AdZAt�A�A=qA�mA�PAoA�A1'A1Ax�A5?A�FA(�A�jA�Av�A(�A��AS�A�`A�uA �A��AO�A$�A
�uA	��A�/AffA��A�A�#AK�A�DA�A�`A^5A
=A��AJA��A��Al�A �@�%@���@���@�@���@�o@�{@���@�+@�\)@�~�@�@��H@�$�@�`B@��@���@�ȴ@�J@�Z@�t�@ް!@�/@��y@�p�@�Z@�n�@�O�@ԃ@�|�@�
=@��@���@�&�@ϕ�@��@Η�@̛�@�b@˥�@��@���@�+@�
=@���@���@ư!@Ƨ�@Ƨ�@Ə\@�v�@�V@�5?@őh@Ĭ@�r�@Ý�@���@�ƨ@�33@�=q@�?}@�X@�I�@�\)@��H@��!@��T@���@��@��@�n�@�=q@��@�@��h@�%@�1'@��m@��
@��w@��P@�|�@�t�@�dZ@�"�@���@���@��m@�;d@��@�ȴ@���@��\@�v�@�^5@�E�@��-@�&�@�  @��w@��H@�~�@�V@��`@��@��w@�l�@�ff@�5?@�{@��^@��h@�O�@�7L@�&�@���@�I�@�C�@��@��@���@�ȴ@��!@���@���@�n�@�^5@�=q@�-@���@���@��^@��@�p�@�?}@��@�%@��@�I�@�A�@�I�@�b@��F@��@��@���@���@�l�@���@���@�n�@�{@���@��h@��/@���@�\)@�@��@��@��y@��!@�~�@�E�@�$�@��T@���@�&�@���@���@�9X@� �@� �@� �@�(�@�b@�dZ@��R@�$�@��/@�t�@�ȴ@��#@�G�@��@�%@��@���@��D@�r�@�Q�@�9X@�  @���@�C�@��@���@���@���@���@���@���@�v�@�ff@�=q@�$�@��@��@��-@�G�@�r�@�|�@�33@��@���@��R@��+@�5?@�{@��@���@��9@�~�@��#@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��#A��/A��/A��;A��yA��A��yA��mA��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A���A���A�  A�A�A�A�A�A�A�A�A�A�A�A�A�%A�%A�%A�%A�%A�1A�1A�
=A�
=A�1A�1A�1A�1A�1A�1A�1A�1A�1A�1A�%A�%A�%A�1A�JA�
=A�JA�JA�JA�JA�
=A�1A�JA�VA�JA�VA�1A�1A�1A�A�%A�%A�
=A�
=A�
=A�JA�VA�JA�JA�1A�
=A�JA�JA�JA�%A�  A���A�  A�A���A��A��A��mA��/A��
A���A�A��^A��RA��FA��FA��9A��9A��-A���A��+A�v�A�jA�l�A�l�A�S�A�+A� �A��A��A��A��A��A��A��A��A��A�{A�{A�{A�oA�VA�A�A�  A���A���A��A��A��yA��yA��mA��yA��mA��TA��A���A���A��RA��9A��-A��A���A���A���A�t�A�;dA���A���A�n�A�;dA�+A�"�A��A�ȴA�ZA�;dA�  A��A��`A���A���A���A��-A��PA��A�n�A�`BA�S�A�9XA�5?A�bA�A��A��A��A��mA��`A��mA��HA��FA���A���A���A��hA�jA�\)A�M�A�;dA�"�A���A��mA�A��-A���A���A�x�A�r�A�l�A�ffA�^5A�ZA�O�A�O�A�K�A�I�A�I�A�G�A�E�A�E�A�E�A�C�A�C�A�G�A�E�A�G�A�G�A�E�A�G�A�E�A�A�A�7LA�1'A�/A�(�A��A��A�%A��^A���A��hA��\A��PA��A�r�A�dZA�^5A�Q�A�O�A�I�A�C�A�=qA�1'A�-A�&�A��A��#A���A���A�r�A�E�A�-A�  A���A���A��yA��RA��7A�`BA�=qA�+A��A�bA�JA�JA�1A�  A��A��yA��HA���A���A��A�p�A�hsA�^5A�S�A�K�A�G�A�E�A�A�A�?}A�=qA�7LA�33A�/A�-A�+A�"�A�  A��A���A�ƨA���A�ĜA��jA�A��RA��9A��!A��!A��!A��-A��-A��A���A��DA��A�bNA�XA�E�A�5?A��A�  A���A��`A��/A���A���A���A�ȴA��wA��^A��FA��-A���A��uA��A�t�A�M�A�;dA�5?A�33A�&�A� �A��A�VA�A���A��A��wA��-A��hA�r�A�K�A�$�A�A���A���A���A���A��PA�x�A�ffA�Q�A�C�A� �A���A��`A��#A���A���A�A��!A���A���A���A���A���A��uA��uA���A���A���A��hA��+A�l�A�Q�A�A�A�1'A�"�A��A�oA�
=A���A��TA��
A�A��\A�jA�\)A�M�A�1'A��A��A�|�A�;dA�
=A��mA��;A��#A���A���A���A��uA��PA�v�A�jA�hsA�`BA�\)A�S�A�I�A�"�A�A��mA��FA�33A�A��A�?}A��mA��wA���A�`BA�=qA��A��;A���A��7A�n�A�XA�Q�A�I�A�7LA�/A�+A�"�A��A�A�  A��TA��RA�p�A�&�A��TA���A�/A��A��A���A��A�`BA�$�A��A�n�A�VA�O�A�G�A�;dA��A��/A��!A���A���A���A�x�A�;dA�bA�A���A��A��A��`A��/A��#A��#A��
A���A�ƨA���A���A��jA��RA��9A��9A��FA��FA��-A���A�l�A�VA��TA���A���A�S�A�-A� �A���A�ȴA�~�A�bNA�`BA�S�A�?}A�&�A�
=A�VA��A�ĜA��+A�ffA�33A�
=A��/A���A��9A��+A��A��A�v�A�hsA�M�A�-A��A��!A�`BA�$�A�oA�oA�VA�
=A���A��#A���A���A���A�ĜA���A��wA��wA��jA��jA��^A��^A��jA��jA��^A��RA��FA���A���A��A�S�A�5?A�ȴA�hsA�O�A�G�A�;dA�9XA�33A�+A��A�A��A��A��/A��
A���A��wA��^A��A���A��\A�z�A�r�A�hsA�XA�G�A�?}A�9XA�-A��A�{A��mA�I�A��TA��!A��+A�ffA�?}A�1A���A��DA�O�A��A�A��yA���A���A�jA�+A���A��mA��A���A�ĜA��FA���A���A���A���A���A��A�v�A�r�A�p�A�p�A�jA�&�A���A���A�~�A�S�A��A�ƨA���A��uA�\)A��!A�(�A���A��hA�=qA���A�{A��A�ƨA��!A��A�A�A��;A���A��A�v�A�9XA�+A�&�A��A��A��A�bA�A���A���A��A��yA��;A���A���A�A��^A���A���A���A���A��uA��hA��hA��PA��DA��+A��A��A��A�r�A�x�A�n�A�`BA�\)A�ZA�S�A�S�A�Q�A�A�A�7LA�=qA�+A��A���A���A��!A��A�A��yA��HA��#A���A�ƨA��-A���A��hA�|�A�ffA�5?A�$�A�
=A��mA��-A��hA�~�A�ffA�^5A�XA�K�A�E�A�=qA�;dA�5?A�(�A�$�A�A��A��;A���A��-A���A��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
��B
��B �B �B;B�BoB�B�B�BBoB�B�B�BoB�B�B{BGB�B�BSB�B�B�B�B0�B=�BP}BT,BTaBTaBTaBWsBl�B�B�B��B�}B� B��B�yB�vB��B�BYB~BCB#nB(�B.B0!B2�B5?B:*B;dB9�B:*B<�BCaB@�BF�BEmB?�BC-BD�B8�B:�B;dB9�B=<B1�B/B(�B'�B!B�B	BIB�BPB	B�B �B��B��B�gB��B�^B�0B�=B�BsBp�BncBk�BjBg�BffBV�BP�BE�B7�B2aB(�B!bB�B
�B
یB
��B
��B
B
��B
��B
�B
�7B
�"B
��B
{B
u�B
p�B
k�B
i�B
_;B
TaB
RTB
NpB
J#B
E�B
>�B
"�B
VB	��B	�JB	�2B	��B	�nB	�B	�FB	�B	��B	��B	r|B	iyB	T�B	A�B	@B	9�B	1[B	'B	!�B	"�B	qB	kB	eB	�B	�B	�B	�B	hB	�B	:B�DB�B��B��B�cB��B�B�B�#BٴB��B�B��B�mB��B�B��B�OB�dB��B�tB�}B�kB�B�	B�	B��B�7B��B��B��B��B��B� B��B��B��B�B��B�~B�DB�B��B}�B|�B{�B{Bx8B~�B.B}�B}�BcB}�B}�B|�B|�B{BzxBy�Bz�B}"B{B{�B{JBz�By�By>Bz�B|�B|�B{JBzxBy	Bv`Bt�BrBpoBm�Bl�Bf2Bh
BiBi�BlWBm]Bl�Bl�Bp;Bo5Bp;BqvBqBp�Bq�Bq�Bt�BuZBt�By	BwfBv�By>B|BxlB{�B~]B� B��B��B�B�SB��B�=B�B�JB��B� B�hB��B��B�_B�eB�+B��B��B�+B��B��B�+B�YB�YB�$B�+B�+B�+B��B��B��B�B��B��B��B��B�B��B�OB��B�B�)B�}B�[B�2B��B��B�B�dB�B��B�B�B�B�B�B�B�&B�B��B�fB��B�`B�B��B��B��B�fB��B��B	�B	+B	�B	DB	
�B	
rB	�B	=B	�B	�B	%FB	$�B	%�B	(�B	)�B	+kB	+kB	+kB	,B	1�B	6zB	6�B	7�B	8�B	8�B	8�B	8�B	8�B	8�B	8�B	9�B	9�B	;0B	<B	<�B	=�B	>B	@B	A�B	B�B	H�B	J#B	J#B	I�B	M6B	R�B	R�B	S&B	S�B	S[B	TaB	X�B	YB	Y�B	[�B	\]B	]�B	a�B	e�B	iDB	jB	j�B	j�B	kB	l�B	m�B	n�B	n�B	poB	qAB	s�B	t�B	v`B	x8B	xB	xB	w�B	w2B	w�B	z�B	}VB	.B	�YB	�B	�bB	��B	��B	��B	��B	��B	�kB	�OB	�!B	��B	��B	�B	��B	�eB	�kB	��B	��B	��B	��B	��B	��B	�wB	��B	�}B	��B	��B	��B	��B	�hB	��B	�qB	��B	��B	� B	��B	B	�3B	��B	�mB	ƨB	ʌB	��B	�
B	��B	یB	ܒG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�5B
�&B
ӏB
�B
��B
��B
��B
�cB;B iB �B;B�B�B
��B
��B
�.B;BBoB �B �B �B iB �B�B;B �BoB�B�B;BoBoB;BoBB�BuBuBAB{BuBABBuB�B�BuB�B�BuBB�BuB�BABB�B;BuBoBoB;BB;B�B�BoB�BB�B{BGB�B{BBABABAB�B�B�BGB{B�BGBGB{BB�B�B�BMB�B_B�B_BMB�BB�BMBoB�B�BYB�BSBuB�B�BB�B�BYBSB+B	lB
�B	lB
�B1BBxB�BB�B�BSBB�B7BkB�B#nB%zB*0B-B0�B1�B1�B1�B2aB2aB2�B8RB=qBEBGBC�BEmBL�BU2BVmBV9BT�BT,BS�BR�BS�BS&BTaBTaBT�BT�BT�BUgBR�BT,BR�BR�BT,BTaBT,BT�BUgBUgBTaBS�BS�BS&BT�BU�BXBXyBW�BXBXyBX�BZ�B]�Bd&Bo�B�YB��B�YB��B�xB�B�bB�IB��B�bB�B�B�@B�XB�RB��B�=B�XB�kB�qB�=B��B��B�'B��B��B�9B��B��B�LB��B�hB��B�XB��B��B��B�jBĜB��B��B�aB�tB�pB�)B��B�NB�B�aB�mB՛B�gB�mB��B��B�KB�BخB�yB�yB�B�B�B�BٴB��BخB�BخB�yB��B�BרB�EB�B�#B�QBںB�WB��B�B�cB�KB�QB�B�B��B�B�AB��B�B�B�B�B�B�B�%B�`BB��B��BoBAB�B�B�B��B��BoBYB
	B
�B
rB	�B	�B�B�B�B�B�B
	B
	BDB\BB�B�B�B	BBB~BOB~BB�B�B 'B�B�B 'B!-B+B%�B%zB'�B%FB(�B(�B%�B'RB)_B*eB)�B)�B(�B(XB(�B,�B*eB+�B,�B,�B0!B/�B1'B2�B-B/�B0!B1�B0�B/B/�B1[B0�B/�B/B0!B2�B2�B3hB7�B4B1[B2�B3hB3�B3�B3hB1�B2�B5�B:�B3hB9�B5�B>wB6FBAUB6B5�B49B;�B<B9XB:�B9XB;dB@�B=�B;0B9�B9�B9$B9�B<�B:�B:*B9�B9�B9�B:^B9�B9XB7�B7�B8�B:�B;�B:*B9�B:�B:�B;�B:^B9XB;0B=qB:�B<B?}B=B=�B;�B@B:*BC�BT�BEBA�BE�BB�B>�BA�BF�BA�B?�B@�BEmBA�B?�B?B?HB?�B>wBC-BB�B?�BFBQ�BAUBB�B[#BI�BF�B@�BK�BF?BA�BJ#BD�B@�BB�B@�B?BA BB�B?}B<�B<�B=B>wB=�BAUBA�BB'BJ#B=�BJ#B>BL0Bl�BA�BA�B@B@OB?HB>BB4�B4�B6B6�B=B:*B:�B7�B7�B7�B:^B>�B=<B;dB;�B:�B:^B<6B;dB<B:�B9�B:�B;dB>B9�B9�B:^B:�B9�B7�B8�B:*B=<BE�BI�B:*B;�B<�B:�B0!B0�B/�B8RB6B/�B-�B.�B1[B.�B*0B-B+6B7�B-�B,=B0!B.�B(�B&LB)_B,=B%B$@B$@B$B%�B'RB.�B)�B/�B%B!B�B�B�B!�B �B�B~BCBB!BCBqB=BBxBqBBkB�BqB�B	B�BIB�B~B%FB=B�B�B�BbBhB�B.B(BPB�B�BxBJB�B
�B
�B	�B
	B_B%B%BMB
�BB�BB�B��B_B�B��B�PB��B��B��B��B��B�GB��B��B�B�BB�KB�2B� B�BBܒB��B�
B�9B��B�2B��B��B��B�aB�[B��B�NB��B�jB̘B�6B�;B��BɆB��B�9B�KB��B�wB��B��BϫB��B��B��B�@B�eB��B�SB�1B�B�~B��B�1B~]B��B�fBz�Bu�Bs�Br�Bq�Bq�Br�Bs�BsMBp�Bp�Bo�BoiBn�Bn�Bm�Bl�BpBncBk�Bm�Bm�BkBkBl�Bj�BjKBkBj�Bj�Bk�BhsBiyBk�BiDBgmBg�BgmBe`Bk�Bd�Bc�Be�Bd&BiB\�Bc�BdZBXyBX�BT�BT,BS�BP}BS�BQBN�BQ�BN<BQ�BI�BK^BJ�BK�B?B>�B>wB:�B:^B8RB6FB7LB5?B33B4�B1�B6FB0�B0�B/�B,qB0�B'�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�.B
�"B
�B
�[B
�B
�B
�aB
�B
�|B
�|B
��B
�GB
�aB
�B
�aB
�GB
�B
�B
�TB
�B
��B
��B
�FB
��B
��BaB}B"�B0UBA�BEBE�BE�BFtBK�BffB��B�VB��B��BĜB��B�B��B�zB�3B�0B �B�BB�B!�B#nB'B,=B/�B.cB,�B/iB5�B9$B9	BDB<B7�BD�B<�B./B-�B-CB/iB5�B'�B&fB�B�BhB�B�B{B�B 4B��B��B��B�B֡B�KB�zB�+B�B��Bz^Be�Bc:B`B]/B\xB[=B]�BJ�BFtB9XB*�B&�BB)BB
�B
ңB
��B
�B
��B
��B
�B
��B
��B
�3B
w�B
nIB
iB
c�B
^�B
a-B
R B
F%B
EB
@�B
<�B
;�B
;�B
EB
MB	��B	��B	�yB	�B	��B	�:B	�)B	��B	��B	�B	m]B	eFB	J�B	4�B	5?B	0oB	&�B	QB	aB	9B	jB	dB	�B	
�B	EB	�B	gB	�B	
�B	
�B��B��B�IB��B�:B�#B�/B�yB͟B�0B̘B�"B�jB͹B�(B�$B��B��B�5B�B��B�&B��B�WB�B��B�dB�~B��B�B��B��B��B��B��B��B�'B��B�4BHB~�B�By	BpUBpUBqvBn�Bo5Bt�BqvBo�Bp!Br�Bp�Bp�BoiBo�Bm�Bm�Bo�BqvBr-Bo5Bn�Bn/BlWBk�Bl�Bo Bq�Bp;Bo BpUBl=Bi�Bg�Bc�Bb�Bd&Bc�B\)B^�B]~B]�B`�B`�B`'Ba�Bh$Bb�BeFBe�Bc�Bc�BdBd�Bh�BhXBiDBl"BjBkQBn�Bp;Bl"Bp�Bq�Br�Bt9Bv�Bv�Bw2By$B~wB~wB~�B��B�{B��B��B�~B�DB�)B�	B�XB�#B��B�lB�7B��B�B�B��B�)B�=B�^B��B��B�@B�SB��B�=B�IB�B�UB��B�hB��B��B�(B�-B�B��B�zB�B̘B�HBңB�uB�@B�uB�B�,B�FBԕB��B�
B�B��B�sB�XB�B�sB�B�B�B�eB�'B�`B�$B��B�VB��B��B		�B	B	�B	�B	�B	�B	�B	kB	qB	�B	�B	/B	�B	$�B	($B	(�B	)yB	)�B	*B	)�B	*0B	*�B	*eB	*B	+6B	+�B	,�B	-�B	.cB	/B	/�B	1�B	3B	5ZB	:xB	;B	;B	;�B	?.B	C�B	DMB	D�B	EB	EB	F�B	J�B	K)B	K�B	M�B	N<B	PbB	T�B	W�B	[#B	[�B	\CB	\]B	\�B	^�B	_�B	`\B	`�B	bNB	cnB	eFB	f�B	hXB	i�B	i_B	i_B	iB	h�B	jeB	mCB	o�B	sB	zDB	�B	�aB	�B	�KB	�B	�	B	�)B	�JB	��B	��B	�oB	��B	�$B	��B	�CB	�/B	�5B	�B	�5B	�B	�5B	�jB	��B	�pB	��B	�BB	��B	��B	��B	�LB	��B	�OB	��B	�-B	�|B	��B	�nB	��B	�zB	�fB	��B	��B	�EB	��B	̘B	�PG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�B
�B
āB
چB
�B
�iB
�B
�;B
�B
�AB
�vB
�B
�B
�|B
��B
�iB
�B
�B
��B
�GB
�B
�B
�vB
�AB
�B
�|B
�B
�B
�GB
�B
�|B
�B
�GB
�GB
�B
�GB
��B
�B
�MB
�MB
�B
�TB
�MB
�B
��B
�MB
�B
�B
�MB
�B
�|B
�MB
��B
�B
�MB
�B
�B
��B
�B
�B
�MB
�GB
�GB
�B
��B
�B
�|B
�B
�GB
�B
��B
�B
�TB
�B
�B
�TB
��B
�B
�B
�B
�B
�B
�B
�B
�TB
�B
�B
�B
�TB
��B
�B
�B
�B
�%B
��B
�8B
��B
�8B
�%B
��B
��B
��B
�%B
�GB
�`B
��B
�2B
�lB
�+B
�MB
��B
�B
��B
��B
�B
�2B
�+B
�B
�DB
��B
�DB
�B
�	B
��B
�PB
��B�B�B�B+B�B
rBBDB�BFBSB	B�B!�B"�B"hB"�B#:B#:B#�B)*B.IB5�B7�B4�B6FB=qBFBGEBGBE�BEBD�BC�BDgBC�BE9BE9BEmBE�BE�BF?BC�BEBC�BC�BEBE9BEBE�BF?BF?BE9BDgBD�BC�BEmBFtBH�BIRBH�BH�BIRBI�BK^BN�BT�B`vBw2Bv`Bw2B~�B|PB}�B�;B�"B��B�:B��B��B�B�B�+B��B�B�B�CB�/B�B��B�ZB��B�fB��B��B�`B�fB�
B�fB�&B��B�B�]B�WB��B�)B�ZB��B��B�B�2B�.B��B��B�B��B�B�+B�YB�%B�+BǔBȚB�	B��B�lB�7B�7B��B�=B�=B�=B�rBʦB�lB�=B�lB�7BɠB��B�fB�B��B��B�B�xB�B�~B�hB�!B�	B�B�qB��BބB��B��B�B�@B��B�nB�nB�tB��B��B�B��B�RB�kB�-B��B��B��B��B�UB�B�-B�B��B��B�0B�^B��B��B�RB��B��B�RB��B��B�B B�BKBKB
�B�B�B�B<BB<B�BpBvB�B�B}B�B�B�B�B9ByBB�BBmBBB#B�B�B�BBKBdB#B]B�BdB �B vB!�B#TB�B �B �B"�B!HB�B �B"B!�B �B�B �B#�B#�B$&B(�B$�B"B#TB$&B$ZB$ZB$&B"�B#TB&�B+QB$&B*KB&fB/5B'B2B&�B&�B$�B,WB,�B*B+�B*B,"B1�B.cB+�B*KB*KB)�B*B-]B+�B*�B*�B*KB*�B+B*�B*B(sB(sB)DB+QB,�B*�B*KB+�B+QB,�B+B*B+�B./B+�B,�B0;B-�B.�B,WB0�B*�B4�BESB5�B2GB6�B3MB/�B2�B7fB2|B0oB1AB6+B2|B0oB/�B0B0�B/5B3�B3MB0�B6�BBuB2B3MBK�B:�B7�B1vB<PB6�B2�B:�B5�B1�B3�B1AB/�B1�B3�B0;B-]B-�B-�B/5B.cB2B2GB2�B:�B.cB:�B.�B<�B]�B2GB2|B0�B1B0B/ B%`B%`B&�B'mB-�B*�B+QB(�B(sB(sB+B/iB-�B,"B,�B+�B+B,�B,"B,�B+�B*KB+�B,"B.�B*�B*KB+B+QB*KB(�B)DB*�B-�B6�B:xB*�B,WB-�B+QB �B!HB �B)B&�B vB�BpB"BpB�B�B�B(sBjB�B �B�B�B
BB�B�B�B�B�BmBBpB�B vB�B�B�B�B�B�B�B�B<BB�B�BB0B�B�B6B0B
�B)B�B0BdB�B	�BB
XB<BB�BgBUB[B B'BUB �B��B�B��B�qB�6B�B�jB��B��B�^B��B�B��B��B�B�dB��B�hB��B��B�B�B�B�UB�B�RB��B�B��B�fB� B�BخB�B�B�#B�
B��B�B�jBɺB��B�BƨB�BĜB��B��B�9B�3B��B�'B��B�BB�qB�B�B��B�^B��B�B�$B��B�OB��B��B��B�}B��B��B�B�=B��B�+By	B|�B}VB��By	Bo5Bs�By>Bk�Bf�BdZBc�Bb�Bb�Bc�Bd�Bd&Ba�Ba|B`�B`BB_pB_�B^jB]�B`�B_;B\�B^�B^�B[�B[�B]�B[�B[#B[�B[�B[�B\]BYKBZQB\�BZBXEBXyBXEBV9B\�BU�BTaBVmBT�BY�BM�BT�BU2BIRBI�BE�BEBD�BAUBDgBA�B?�BB�B?BB[B:�B<6B;�B<�B/�B/�B/iB+�B+QB)DB'8B(>B&2B$&B%�B"�B'8B!�B!�B �BdB!|By111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<P�^<#�
<#�
<��V<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<9�P<��<%�C<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'Z�<^v<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Ek�<#�
<#�
<#�
<��M<�q�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<$�&<3'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<6�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OW V1.0: r =0.9996(+/-0.0001), vertically averaged dS =-0.0145(+/-0.0031)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      NO correction for Conductivity Thermal Mass (CTM) is applied;    OW V1.0: r =0.9996(+/-0.0001), vertically averaged dS =-0.0145(+/-0.0031)                                                                                                                      SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OW weighted least squares fit is adopted; Map Scales:[x:8/4,y:4/2.0]; Fit T>2.1C; max_breaks=1;                                                                    PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OW weighted least squares fit is adopted; Map Scales:[x:8/4,y:4/2.0]; Fit T>2.1C; max_breaks=1;                                                                    PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     202102122209382021021222093820210212220938202102122209382021021222093820210212220938SI  SI  ARFMARFM                                                                                                                                                2020031911492320200319114923IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020031912003020200319120030QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020031912003020200319120030QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020060109060320200601090603IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021021222095320210212220953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2020V01                                            CTD_for_DMQC_2020V01                                            2021021222095320210212220953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021021222095320210212220953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                