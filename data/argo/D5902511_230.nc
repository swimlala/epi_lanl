CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-09-26T03:08:49Z creation; 2023-02-10T23:09:45Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     0  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   U8   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     0  [D   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   st   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     0  y�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0  �X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 "�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ;    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ;`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   A`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   G`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T M`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   M�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   M�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   M�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   M�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � M�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   NT   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   Np   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Nx   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        N�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        N�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       N�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    N�Argo profile    3.1 1.2 19500101000000  20220926030849  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_230                 6810_008521_230                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��m	�@��m	�11  @�񁙙��@�񁙙��@1��KHӮ@1��KHӮ�d�|��G��d�|��G�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@�G�@��R@��R@�  AG�A  A   A,(�A?\)A_\)A�  A�  A�\)A��A��A�\)A߮A��B   B�B�
B�
B�
B'�
B0(�B8Q�B@(�BH(�BP(�BX  B_�Bg�
Bp  Bx(�B�  B��B��B�  B�(�B��B��
B��B��B��B��B��B�  B��B�{B�Q�B�  B�  B��B��
B��B�  B�{B�{B�  B�  B�{B�{B�  B�  B�{B�(�C 
=C��C��C��C
=C
  C��C  C  C��C  C��C  C  C��C��C   C"
=C$  C&  C(
=C*  C+��C.  C0{C2  C3��C6
=C8
=C:  C<
=C>  C@  CB
=CC��CE�HCG��CJ
=CK��CN  CO��CR
=CT  CU��CX  CZ  C\  C]��C`
=Ca��Cc��Cf  Ch
=Cj  Ck��Cm�Cp  Cr  Ct  Cv  Cx
=Cz  C|  C~  C�C���C�
=C�C�  C���C���C�C�
=C�  C�  C�  C�  C�  C�
=C�  C�C���C���C���C��C�  C�
=C�
=C�C�C�
=C�
=C�C�C�C�  C�  C���C���C���C�  C���C���C���C���C�  C�
=C�
=C�  C�  C�C�  C�C�  C�  C���C�  C���C�  C�  C�C�  C���C�  C���C���C�  C�  C�  C�  C���C�  C�  C�  C�  C�C�  C���C�C�C�  C���C���C���C���C�  C�  C���C��C���C�  C���C���C�C�C���C���C�  C�C���C�  C�C�C�C�C�C�C���C���C�  C�C�  C�  C�
=C�  C�  C�C�C�
=C�C�  C�  C���C���C�  C���C�  C���C���C���C�C�D   D � D  D� D  D� D�D�D�D� D  D��D�D��D  D}qD�qD� D	  D	z�D
  D
��D  D}qD  D}qD�D��D  D� D  D�DD�D�D��D�D}qD��Dz�D�qD� D  D}qD�qD��D�Dz�D  D��D  D��D  D}qD�qD}qD  D}qD�qD� D  D� D�D}qD�qD � D!  D!��D"�D"��D#�D#��D#�qD$}qD%  D%z�D%��D&� D'�D'��D(  D(��D)�D)��D*  D*z�D*�qD+� D,�D,��D-D-� D.  D.�D/D/��D/�qD0� D1  D1��D2  D2��D3  D3� D4�D4��D5  D5��D6�D6}qD7  D7��D8  D8� D9�D9� D:  D:��D;  D;� D<  D<� D<�qD=� D>  D>� D?�D?��D@�D@��DA  DA��DB�DB� DC�DC��DD  DD� DE  DE� DF�DF��DG  DG}qDH  DH� DH�qDI}qDJ  DJ}qDJ�qDK}qDK�qDL� DM  DM� DN  DN� DO  DO� DP  DP}qDP�qDQ� DR�DR�DS�DS� DS�qDT� DU  DU� DV  DV��DW  DW}qDX  DX� DY�DY� DY�qDZ� DZ�qD[}qD\  D\� D]�D]��D^D^�D_�D_� D`  D`}qDa�Da}qDa��Db}qDb�qDc}qDc��Dd� De  Dez�De�qDf}qDg  Dg� Dh�Dh� Dh��Di}qDj  Dj}qDj��Dk}qDk�qDl}qDl��Dm}qDn�Dn� Do  Do� Do�qDp� Dq�Dq�DrDr� Dr�qDs}qDs�qDt}qDt�qDu� Dv�Dv��Dw�Dw��Dx  Dx}qDy  Dy� Dy�qDzz�Dz�qD{� D{��D|� D}  D}}qD~  D~� D  D� D�qD�@ D�� D���D�  D�AHD�~�D���D���D�@ D��HD�� D�  D�@ D���D�D�  D�>�D�~�D��HD�  D�>�D�~�D�� D�  D�@ D���D�� D���D�>�D�~�D��HD�HD�>�D�� D�D�  D�>�D�� D�� D�HD�AHD�� D���D���D�>�D�~�D�� D�  D�B�D�q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?.{?�=q?�p�?��@�\@�@+�@:�H@Q�@c�
@xQ�@�ff@�{@�(�@��\@���@�
=@��
@�{@�Q�@��
@���@��HA�\A�Ap�A�A��A��A#33A'
=A.{A2�\A8��A>�RAC33AI��AN{AU�AX��A`  Adz�Ai��AqG�Au�A|(�A�Q�A�33A�ffA���A��A��RA���A�z�A��RA�=qA�(�A��A�=qA���A�  A��A�A�  A��HA�{A�Q�A��A�ffA���A�(�A�ffA�=qA�z�A�\)A��A�z�A�  A��A��A߮A�\A�A�\)A��HA�p�A�\)A��HA��A�  A��HA���B (�Bp�BffB(�BG�B�RB(�B	�B
�\BQ�BG�B�\BQ�BG�B
=B(�Bp�B
=B  BB
=BQ�B�B
=B ��B!B#�B$��B&{B'�B(��B*ffB+\)B-�B.{B/�B0��B2=qB3�B4��B6ffB7�B9�B:{B<  B<��B>�\B?�BAG�BB�\BC�BEp�BF=qBG�
BIG�BJ=qBK�
BM�BN=qBP  BQ�BR�\BS�
BT��BV�\BW�BX��BZffB[\)B]�B]�B_�B`��BaBc\)Bd��Be�Bg\)Bh(�Bi�Bj�HBl  Bm��Bn�\BpQ�Bq�BrffBs�
Bt��BvffBw�
Bx��Bz{B{\)B|(�B}�B~�HB�
B��RB��B��
B�ffB���B��B�{B���B�p�B�(�B��HB�\)B�{B��RB�G�B�{B��\B�G�B��B�ffB�33B��B�Q�B��B���B�Q�B��HB���B�Q�B��HB�\)B�=qB��RB�\)B�{B��\B�G�B�B�z�B��B��B�ffB��HB�B�(�B���B���B�{B��RB���B�(�B���B�p�B�{B��\B�p�B��
B��\B�G�B�B��\B�33B��B�z�B���B���B�ffB���B��B�ffB��HB��B�Q�B��HB�\)B�=qB��RB�p�B�(�B��\B�G�B�  B�z�B���B��B�Q�B��RB�\)B�  B�Q�B���B��B��
B�=qB��HB�
=B���B�  B�Q�B���B��HB�\)B�B��
B�(�B��\B��RB���B�p�B��B��B�{B�z�B��\B���B�\)B�p�B��
B�=qB�Q�B¸RB��B�33BîB��B�=qBģ�B���B�
=BŅB��
B�  B�Q�B���B�
=B�G�B�B�  B�{B�z�B���B��B�\)B��
B�{B�=qBʸRB�
=B�33BˮB��B�(�Ḅ�B��HB��BͅB��B�=qB�ffB��HB�33B�p�B��B�Q�B�ffB���B�G�B�p�B��B�=qB�ffB���B�G�BӅB�B�=qB�ffBԸRB�33Bՙ�B��
B�{B֏\B���B���B�p�B��
B�  B�ffB���B��HB�33BٮB��B�(�Bڣ�B���B�
=Bۙ�B��
B�{B�z�B��HB�
=B݅B��
B�  Bޏ\B���B���B߅B��B�{B�z�B��HB�33B�p�B��B�Q�B�\B��HB�\)B�B��B�=qB�RB�33B�\)B�B�(�B�Q�B�RB��B�\)B癚B�(�B�z�B�\B�
=B�B�B�  B�ffB���B�
=B�B��B�{B�ffB�RB�G�B�B�B�=qB��B��HB�\)B��
B�{B�Q�B��HB�33B�p�B��
B�ffB�RB��HB�p�B��B�{B�z�B���B�p�B��B�{B��RB���B�G�B��
B�=qB�z�B��B�p�B�B�=qB��RB�
=B�\)B��B�ffB��RB��B��B��B�Q�B���B�\)B��C {C \)C �C �C  C=qCffC��C�C{C=qCz�C��C  C(�Cz�CC�C�Cz�C�RC�HC33Cp�C��C�HC=qCffC��C�HC33C\)C��C��C(�CQ�C��C�HC	{C	p�C	�C	�
C
�C
p�C
�C
�
C33Cp�C��C�HC33CffC��C��C33C\)C�RC��C�Cz�C�RC�C=qCz�C�C  CQ�C�C�RC
=C=qCp�C��C��C(�C�C��C�C=qCffC�C  C(�Cp�C��C�C(�C�C�C�CG�Cz�C�C
=C=qCffC��C  C33C�CC�CG�C�C�C  CG�Cp�C�RC{C=qCz�C��C  CG�C�\CC  CQ�Cz�CC�CG�Cz�CC{CG�Cz�C�C   C G�C �C �RC �C!=qC!�C!�RC!�C"33C"�C"C"��C#33C#�\C#C#��C$33C$�C$��C%
=C%G�C%z�C%�RC&
=C&G�C&z�C&��C&��C'=qC'z�C'�C'�HC(�C(ffC(�C(�HC)
=C)Q�C)��C)�C*(�C*\)C*�\C*�
C+(�C+p�C+�RC+�HC,�C,Q�C,��C,�HC-33C-Q�C-�\C-��C.�C.\)C.��C.C/  C/Q�C/��C/�
C0{C0G�C0�C0��C1�C1\)C1��C1�
C2{C2Q�C2��C2�HC3(�C3p�C3��C3�HC4�C4ffC4�RC4�C533C5ffC5�C6  C6G�C6�\C6C6��C7=qC7�C7��C8{C8G�C8�C8C9  C9Q�C9��C9��C:�C:ffC:��C:�HC;33C;�C;�C;�C<33C<�C<C<�C==qC=�\C=�
C>
=C>G�C>��C>�C?�C?\)C?�RC@
=C@Q�C@�\C@��CA
=CAG�CA��CA�HCB33CBz�CBCB��CC33CCz�CC��CD
=CD=qCD�CD�HCE(�CE\)CE��CE�
CF�CF\)CF�CG  CGG�CG�\CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                              111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @   @B�\@�G�@��R@��R@�  AG�A  A   A,(�A?\)A_\)A�  A�  A�\)A��A��A�\)A߮A��B   B�B�
B�
B�
B'�
B0(�B8Q�B@(�BH(�BP(�BX  B_�Bg�
Bp  Bx(�B�  B��B��B�  B�(�B��B��
B��B��B��B��B��B�  B��B�{B�Q�B�  B�  B��B��
B��B�  B�{B�{B�  B�  B�{B�{B�  B�  B�{B�(�C 
=C��C��C��C
=C
  C��C  C  C��C  C��C  C  C��C��C   C"
=C$  C&  C(
=C*  C+��C.  C0{C2  C3��C6
=C8
=C:  C<
=C>  C@  CB
=CC��CE�HCG��CJ
=CK��CN  CO��CR
=CT  CU��CX  CZ  C\  C]��C`
=Ca��Cc��Cf  Ch
=Cj  Ck��Cm�Cp  Cr  Ct  Cv  Cx
=Cz  C|  C~  C�C���C�
=C�C�  C���C���C�C�
=C�  C�  C�  C�  C�  C�
=C�  C�C���C���C���C��C�  C�
=C�
=C�C�C�
=C�
=C�C�C�C�  C�  C���C���C���C�  C���C���C���C���C�  C�
=C�
=C�  C�  C�C�  C�C�  C�  C���C�  C���C�  C�  C�C�  C���C�  C���C���C�  C�  C�  C�  C���C�  C�  C�  C�  C�C�  C���C�C�C�  C���C���C���C���C�  C�  C���C��C���C�  C���C���C�C�C���C���C�  C�C���C�  C�C�C�C�C�C�C���C���C�  C�C�  C�  C�
=C�  C�  C�C�C�
=C�C�  C�  C���C���C�  C���C�  C���C���C���C�C�D   D � D  D� D  D� D�D�D�D� D  D��D�D��D  D}qD�qD� D	  D	z�D
  D
��D  D}qD  D}qD�D��D  D� D  D�DD�D�D��D�D}qD��Dz�D�qD� D  D}qD�qD��D�Dz�D  D��D  D��D  D}qD�qD}qD  D}qD�qD� D  D� D�D}qD�qD � D!  D!��D"�D"��D#�D#��D#�qD$}qD%  D%z�D%��D&� D'�D'��D(  D(��D)�D)��D*  D*z�D*�qD+� D,�D,��D-D-� D.  D.�D/D/��D/�qD0� D1  D1��D2  D2��D3  D3� D4�D4��D5  D5��D6�D6}qD7  D7��D8  D8� D9�D9� D:  D:��D;  D;� D<  D<� D<�qD=� D>  D>� D?�D?��D@�D@��DA  DA��DB�DB� DC�DC��DD  DD� DE  DE� DF�DF��DG  DG}qDH  DH� DH�qDI}qDJ  DJ}qDJ�qDK}qDK�qDL� DM  DM� DN  DN� DO  DO� DP  DP}qDP�qDQ� DR�DR�DS�DS� DS�qDT� DU  DU� DV  DV��DW  DW}qDX  DX� DY�DY� DY�qDZ� DZ�qD[}qD\  D\� D]�D]��D^D^�D_�D_� D`  D`}qDa�Da}qDa��Db}qDb�qDc}qDc��Dd� De  Dez�De�qDf}qDg  Dg� Dh�Dh� Dh��Di}qDj  Dj}qDj��Dk}qDk�qDl}qDl��Dm}qDn�Dn� Do  Do� Do�qDp� Dq�Dq�DrDr� Dr�qDs}qDs�qDt}qDt�qDu� Dv�Dv��Dw�Dw��Dx  Dx}qDy  Dy� Dy�qDzz�Dz�qD{� D{��D|� D}  D}}qD~  D~� D  D� D�qD�@ D�� D���D�  D�AHD�~�D���D���D�@ D��HD�� D�  D�@ D���D�D�  D�>�D�~�D��HD�  D�>�D�~�D�� D�  D�@ D���D�� D���D�>�D�~�D��HD�HD�>�D�� D�D�  D�>�D�� D�� D�HD�AHD�� D���D���D�>�D�~�D�� D�  D�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?.{?�=q?�p�?��@�\@�@+�@:�H@Q�@c�
@xQ�@�ff@�{@�(�@��\@���@�
=@��
@�{@�Q�@��
@���@��HA�\A�Ap�A�A��A��A#33A'
=A.{A2�\A8��A>�RAC33AI��AN{AU�AX��A`  Adz�Ai��AqG�Au�A|(�A�Q�A�33A�ffA���A��A��RA���A�z�A��RA�=qA�(�A��A�=qA���A�  A��A�A�  A��HA�{A�Q�A��A�ffA���A�(�A�ffA�=qA�z�A�\)A��A�z�A�  A��A��A߮A�\A�A�\)A��HA�p�A�\)A��HA��A�  A��HA���B (�Bp�BffB(�BG�B�RB(�B	�B
�\BQ�BG�B�\BQ�BG�B
=B(�Bp�B
=B  BB
=BQ�B�B
=B ��B!B#�B$��B&{B'�B(��B*ffB+\)B-�B.{B/�B0��B2=qB3�B4��B6ffB7�B9�B:{B<  B<��B>�\B?�BAG�BB�\BC�BEp�BF=qBG�
BIG�BJ=qBK�
BM�BN=qBP  BQ�BR�\BS�
BT��BV�\BW�BX��BZffB[\)B]�B]�B_�B`��BaBc\)Bd��Be�Bg\)Bh(�Bi�Bj�HBl  Bm��Bn�\BpQ�Bq�BrffBs�
Bt��BvffBw�
Bx��Bz{B{\)B|(�B}�B~�HB�
B��RB��B��
B�ffB���B��B�{B���B�p�B�(�B��HB�\)B�{B��RB�G�B�{B��\B�G�B��B�ffB�33B��B�Q�B��B���B�Q�B��HB���B�Q�B��HB�\)B�=qB��RB�\)B�{B��\B�G�B�B�z�B��B��B�ffB��HB�B�(�B���B���B�{B��RB���B�(�B���B�p�B�{B��\B�p�B��
B��\B�G�B�B��\B�33B��B�z�B���B���B�ffB���B��B�ffB��HB��B�Q�B��HB�\)B�=qB��RB�p�B�(�B��\B�G�B�  B�z�B���B��B�Q�B��RB�\)B�  B�Q�B���B��B��
B�=qB��HB�
=B���B�  B�Q�B���B��HB�\)B�B��
B�(�B��\B��RB���B�p�B��B��B�{B�z�B��\B���B�\)B�p�B��
B�=qB�Q�B¸RB��B�33BîB��B�=qBģ�B���B�
=BŅB��
B�  B�Q�B���B�
=B�G�B�B�  B�{B�z�B���B��B�\)B��
B�{B�=qBʸRB�
=B�33BˮB��B�(�Ḅ�B��HB��BͅB��B�=qB�ffB��HB�33B�p�B��B�Q�B�ffB���B�G�B�p�B��B�=qB�ffB���B�G�BӅB�B�=qB�ffBԸRB�33Bՙ�B��
B�{B֏\B���B���B�p�B��
B�  B�ffB���B��HB�33BٮB��B�(�Bڣ�B���B�
=Bۙ�B��
B�{B�z�B��HB�
=B݅B��
B�  Bޏ\B���B���B߅B��B�{B�z�B��HB�33B�p�B��B�Q�B�\B��HB�\)B�B��B�=qB�RB�33B�\)B�B�(�B�Q�B�RB��B�\)B癚B�(�B�z�B�\B�
=B�B�B�  B�ffB���B�
=B�B��B�{B�ffB�RB�G�B�B�B�=qB��B��HB�\)B��
B�{B�Q�B��HB�33B�p�B��
B�ffB�RB��HB�p�B��B�{B�z�B���B�p�B��B�{B��RB���B�G�B��
B�=qB�z�B��B�p�B�B�=qB��RB�
=B�\)B��B�ffB��RB��B��B��B�Q�B���B�\)B��C {C \)C �C �C  C=qCffC��C�C{C=qCz�C��C  C(�Cz�CC�C�Cz�C�RC�HC33Cp�C��C�HC=qCffC��C�HC33C\)C��C��C(�CQ�C��C�HC	{C	p�C	�C	�
C
�C
p�C
�C
�
C33Cp�C��C�HC33CffC��C��C33C\)C�RC��C�Cz�C�RC�C=qCz�C�C  CQ�C�C�RC
=C=qCp�C��C��C(�C�C��C�C=qCffC�C  C(�Cp�C��C�C(�C�C�C�CG�Cz�C�C
=C=qCffC��C  C33C�CC�CG�C�C�C  CG�Cp�C�RC{C=qCz�C��C  CG�C�\CC  CQ�Cz�CC�CG�Cz�CC{CG�Cz�C�C   C G�C �C �RC �C!=qC!�C!�RC!�C"33C"�C"C"��C#33C#�\C#C#��C$33C$�C$��C%
=C%G�C%z�C%�RC&
=C&G�C&z�C&��C&��C'=qC'z�C'�C'�HC(�C(ffC(�C(�HC)
=C)Q�C)��C)�C*(�C*\)C*�\C*�
C+(�C+p�C+�RC+�HC,�C,Q�C,��C,�HC-33C-Q�C-�\C-��C.�C.\)C.��C.C/  C/Q�C/��C/�
C0{C0G�C0�C0��C1�C1\)C1��C1�
C2{C2Q�C2��C2�HC3(�C3p�C3��C3�HC4�C4ffC4�RC4�C533C5ffC5�C6  C6G�C6�\C6C6��C7=qC7�C7��C8{C8G�C8�C8C9  C9Q�C9��C9��C:�C:ffC:��C:�HC;33C;�C;�C;�C<33C<�C<C<�C==qC=�\C=�
C>
=C>G�C>��C>�C?�C?\)C?�RC@
=C@Q�C@�\C@��CA
=CAG�CA��CA�HCB33CBz�CBCB��CC33CCz�CC��CD
=CD=qCD�CD�HCE(�CE\)CE��CE�
CF�CF\)CF�CG  CGG�CG�\CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                              111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�ZA�ZA�XA�=qA�A���A�|�A�G�A���A���Aߴ9Aߙ�A߃A�|�A�x�A�r�A�jA�ffA�dZA�`BA�\)A�ZA�XA�XA�S�A�S�A�Q�A�O�A�M�A�K�A�I�A�I�A�G�A�I�A�I�A�G�A�=qA�VA�/A��AܼjA�G�A��HA���A�ĜAۼjA۸RAۼjAۡ�AۋDA�l�A��A���Aڟ�A��A��A��A���Aѝ�A�ĜA�hsA���AɮA��Aƛ�A�M�A�I�A���A��PA�O�A�oA��A�oA�ƨA�z�A��A���A���A�=qA�\)A�  A�+A��+A�G�A���A�{A�&�A��+A���A��A�+A��9A�M�A��/A��A��!A�^5A��+A��yA�M�A�C�A�VA��PA�1'A��\A�/A�r�A�|�A��A��A�VA�\)A�~�A�bNA�JA�ffA�C�A�1A��FA��A��A�&�A�l�A�oA�bA�"�A�9XA�A���A��
A�-A���A�
A|JAvĜAu?}ArbNAo|�An�AjVAe�;Aa+A]l�AZ�AXv�AT��ARI�AQ��AQ+AO�
AN^5AKK�AH �AFffAE;dAB(�A@�A>��A=�A;/A8�+A8bA7\)A6�+A5C�A3G�A1XA/�A-�
A-`BA,��A, �A*��A)G�A(ȴA(1A&�A$��A"�!A!x�AXAA�uAQ�A�A��AȴA�PA�FA��AM�A��A��A(�AbAA&�A-A�AdZAAv�AE�A�7A$�AjA
=A �A�A
�A	+A�Al�A(�A�wA�^A^5A33AbA��A~�A��A I�@���@�;dA A (�A=qAn�A�
A��AĜA�7A ~�A(�AA�#A��At�A ��@��H@���@�{@��@��u@� �@���@���@���@���@�@�I�@�\@�%@�%@�"�@�^5@�Ĝ@�bN@��@��@��@�x�@�b@�;d@��y@�x�@��@�X@ڏ\@�@��@ם�@�5?@�@ՙ�@�x�@ԓu@��@ҸR@�E�@�$�@�@��T@��@�1'@Ͼw@���@�ff@�^5@�V@�?}@̋D@�V@�p�@���@���@�$�@�@�p�@�A�@˶F@˅@�t�@�C�@ɡ�@ǥ�@��y@��@ũ�@ģ�@�r�@�bN@Ý�@°!@���@�(�@��w@��;@�S�@�V@�$�@�$�@��@��@��+@�v�@�ff@�^5@�M�@���@�%@��@��m@�\)@���@�v�@�ff@�V@���@��@�z�@�9X@��;@���@��+@�x�@��/@��@�Q�@���@�S�@�K�@�;d@��R@��T@��^@�X@��@��@�r�@�bN@�1'@��
@��F@��@�o@��7@�G�@�?}@�%@�V@��`@���@�bN@��m@��@�33@�V@��@���@���@�G�@�/@��9@��@�S�@�"�@���@��@���@�O�@��@� �@��w@�S�@��R@�ff@��@��h@�`B@���@���@�1@���@���@��P@�dZ@�dZ@�S�@�ȴ@��+@�ff@�=q@�$�@���@��@�O�@��@���@�A�@�9X@��m@���@��@�;d@��P@���@�@�^5@�J@��#@�@���@�X@�?}@�&�@�%@��j@�Q�@��m@�\)@�S�@�S�@�+@��H@�n�@�E�@�-@���@��7@�G�@�V@��@�A�@�1'@��@��@��@�S�@��@���@���@��+@�^5@���@��@��@��@�hs@�`B@���@��7@��@��/@���@��@�j@��@�r�@��@�t�@�33@��H@��\@�v�@�ff@�=q@�-@�=q@�-@�J@��#@�@���@�G�@�V@���@��`@���@��9@��D@�r�@�I�@��;@��F@�t�@�;d@��y@���@��+@�n�@�=q@�@���@�X@��@��j@�bN@�A�@�9X@�(�@� �@�  @��F@���@�dZ@�K�@�C�@��@��y@���@�n�@�=q@�{@�@��^@�?}@��@��@�V@��@���@��u@�j@�A�@��@��m@���@��P@�K�@��H@��R@�n�@�{@��@���@�p�@�7L@�V@��9@�Z@�1'@� �@�@\)@~��@~�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�VA�XA�^5A�VA�\)A�ZA�VA�O�A�`BA�`BA�XA�1'A�{A�VA���A�  A��mA���A���A�t�A��\A�n�A�M�A�ZA�"�A���A��A��;A�ĜA�ȴA߾wA߲-Aߣ�Aߡ�Aߛ�Aߗ�Aߏ\A߅A߉7A�~�A߅A�~�A�|�A߁A�z�A�~�A�z�A�|�A�|�A�v�A�z�A�z�A�x�A�z�A�t�A�x�A�r�A�t�A�p�A�l�A�p�A�hsA�n�A�hsA�jA�jA�ffA�jA�ffA�dZA�dZA�`BA�ffA�`BA�dZA�dZA�bNA�ffA�^5A�bNA�^5A�`BA�bNA�\)A�`BA�^5A�ZA�^5A�ZA�ZA�\)A�XA�^5A�ZA�VA�ZA�VA�XA�XA�S�A�VA�ZA�XA�VA�ZA�S�A�ZA�VA�VA�VA�O�A�S�A�Q�A�Q�A�VA�O�A�VA�O�A�VA�O�A�VA�S�A�S�A�S�A�M�A�S�A�M�A�Q�A�O�A�O�A�O�A�M�A�O�A�K�A�Q�A�I�A�O�A�I�A�M�A�I�A�M�A�K�A�I�A�M�A�G�A�K�A�K�A�E�A�K�A�I�A�G�A�M�A�E�A�I�A�G�A�G�A�K�A�E�A�I�A�I�A�G�A�K�A�E�A�K�A�K�A�G�A�K�A�I�A�I�A�K�A�E�A�K�A�G�A�G�A�K�A�E�A�K�A�E�A�G�A�G�A�?}A�A�A�A�A�9XA�;dA�;dA�1'A�5?A�-A�{A�A��Aޥ�Aޏ\Aޏ\Aމ7A�(�A�ƨAݍPA�Q�A�=qA��A�bA�A��A��A��mA��AܾwAܩ�Aܡ�A܇+A�p�A�hsA�XA�Q�A�A�A�VA��A��`A��/A��;A��A��A��#A���A��A���A���A���A���A�ƨA���A�ĜA���A���A�ĜA�A�Aۺ^A۸RA۶FAۼjA۸RA۸RAۼjA۶FAۺ^Aۺ^A۶FAۼjAۼjA۸RA۾wAۺ^AۼjA���A۰!Aۗ�Aە�AۓuAۑhAۗ�AۓuAۍPAۍPAۅAۃAۅA�|�A�z�A�t�A�^5A�Q�A�O�A�O�A�A�A�33A�+A�VA�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��TA��A���A���A�ƨAڶFAڙ�AڅA�p�A�ffA�K�A�jA�
=A�A�`BA��A��A���A�AֶFA֣�A֓uA�~�A�Q�A�5?A� �A�"�A�{A�bA�A���A��A��`A���A�ĜAռjAպ^AռjAոRAլA՟�AՋDA�jA�ZA�1'A�{A��`A���AԲ-A�
=AӮA�z�A�E�A�A���A�ƨA�ƨAҼjAҝ�AҋDA�x�A�ffA�`BA�\)A�Q�A�C�A�-A�oA�
=A���A�Q�A��/A�ƨAа!AС�AЕ�AЉ7A�S�A�=qA��A��A�ĜAϬAρA�VA�?}A�1'A���Aβ-A�dZA�S�A�$�AͼjA�S�A� �A���A��
A̼jA̲-ÁA�-A���A˲-Aˏ\A�p�A�A�A���Aʴ9A�5?A�1A��yA��/A���A�ȴAɼjAɮAɬAɣ�Aɝ�Aɟ�Aɟ�Aə�Aɛ�Aɟ�Aɛ�Aɥ�Aɧ�AɬA�~�A��A��
AȾwAȓuA�z�A�jA�+A�JA��A���A�;dAƋDA�5?A�A���AŶFAŃA� �A�1A�ĜAāA�ZA�A�A�+A��A�JA�  A��A��;A��#A���AîAã�AÇ+A�K�A��A�A°!APA�`BA�K�A�"�A���A��DA�dZA�Q�A�G�A�33A��A�1A���A��A���A���A��\A�^5A��A�
=A��A�ĜA���A��hA��A�t�A�ZA�{A��A�O�A�?}A�-A�+A�-A�$�A��A�%A��A���A��RA�C�A�JA��
A��FA��-A���A�z�A�`BA�A�A�+A�JA���A��yA��/A��#A���A���A�A�ȴA�ĜA���A�ĜA��wA��-A���A��hA�n�A�hsA�ffA�^5A�O�A�O�A�G�A�A�A�/A��A�  A��HA���A���A��^A�x�A��-A�5?A��!A�O�A�%A���A��FA���A���A��uA��A�t�A�l�A�S�A�?}A�=qA�7LA�1'A�33A�+A�&�A�&�A�oA���A�v�A� �A��A���A�5?A�JA�1A�A�  A���A���A��A��
A��A��A�r�A�O�A�$�A��/A�z�A�1'A�VA��`A��A�r�A�G�A�5?A��A���A���A���A�C�A�VA�  A��A��`A��
A��wA��FA��!A��A���A��DA�z�A�z�A�jA�Q�A�9XA�"�A�A��TA���A��+A�S�A�5?A�$�A� �A��A��A�bA���A��TA���A���A�|�A�`BA�G�A�33A��A��A�oA�
=A�  A���A��mA���A�ĜA���A��hA�ffA��wA��7A�&�A���A���A��7A�x�A�hsA�=qA��A���A��A�A��wA��jA��jA��RA��A���A���A���A���A��\A�~�A�n�A�ZA�E�A��A��TA�A��A�/A�ĜA���A���A���A���A��uA��PA��DA��7A�~�A�t�A�p�A�dZA�K�A�
=A���A���A��DA�|�A�K�A��A��`A���A�z�A�Q�A�+A��A�bA�1A���A��mA��^A���A�t�A�9XA��;A���A�~�A�XA�ƨA��A��A�z�A�p�A�hsA�ZA�XA�M�A�VA�I�A�5?A�  A��yA��FA�~�A��DA��A�|�A� �A�A���A��PA�jA�;dA���A���A��9A���A���A��hA��DA��A�v�A�dZA�VA�E�A�9XA�1'A�-A�{A���A��;A���A�~�A�z�A�r�A�r�A�r�A�v�A�v�A�n�A�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                              111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ZA�ZA�XA�=qA�A���A�|�A�G�A���A���Aߴ9Aߙ�A߃A�|�A�x�A�r�A�jA�ffA�dZA�`BA�\)A�ZA�XA�XA�S�A�S�A�Q�A�O�A�M�A�K�A�I�A�I�A�G�A�I�A�I�A�G�A�=qA�VA�/A��AܼjA�G�A��HA���A�ĜAۼjA۸RAۼjAۡ�AۋDA�l�A��A���Aڟ�A��A��A��A���Aѝ�A�ĜA�hsA���AɮA��Aƛ�A�M�A�I�A���A��PA�O�A�oA��A�oA�ƨA�z�A��A���A���A�=qA�\)A�  A�+A��+A�G�A���A�{A�&�A��+A���A��A�+A��9A�M�A��/A��A��!A�^5A��+A��yA�M�A�C�A�VA��PA�1'A��\A�/A�r�A�|�A��A��A�VA�\)A�~�A�bNA�JA�ffA�C�A�1A��FA��A��A�&�A�l�A�oA�bA�"�A�9XA�A���A��
A�-A���A�
A|JAvĜAu?}ArbNAo|�An�AjVAe�;Aa+A]l�AZ�AXv�AT��ARI�AQ��AQ+AO�
AN^5AKK�AH �AFffAE;dAB(�A@�A>��A=�A;/A8�+A8bA7\)A6�+A5C�A3G�A1XA/�A-�
A-`BA,��A, �A*��A)G�A(ȴA(1A&�A$��A"�!A!x�AXAA�uAQ�A�A��AȴA�PA�FA��AM�A��A��A(�AbAA&�A-A�AdZAAv�AE�A�7A$�AjA
=A �A�A
�A	+A�Al�A(�A�wA�^A^5A33AbA��A~�A��A I�@���@�;dA A (�A=qAn�A�
A��AĜA�7A ~�A(�AA�#A��At�A ��@��H@���@�{@��@��u@� �@���@���@���@���@�@�I�@�\@�%@�%@�"�@�^5@�Ĝ@�bN@��@��@��@�x�@�b@�;d@��y@�x�@��@�X@ڏ\@�@��@ם�@�5?@�@ՙ�@�x�@ԓu@��@ҸR@�E�@�$�@�@��T@��@�1'@Ͼw@���@�ff@�^5@�V@�?}@̋D@�V@�p�@���@���@�$�@�@�p�@�A�@˶F@˅@�t�@�C�@ɡ�@ǥ�@��y@��@ũ�@ģ�@�r�@�bN@Ý�@°!@���@�(�@��w@��;@�S�@�V@�$�@�$�@��@��@��+@�v�@�ff@�^5@�M�@���@�%@��@��m@�\)@���@�v�@�ff@�V@���@��@�z�@�9X@��;@���@��+@�x�@��/@��@�Q�@���@�S�@�K�@�;d@��R@��T@��^@�X@��@��@�r�@�bN@�1'@��
@��F@��@�o@��7@�G�@�?}@�%@�V@��`@���@�bN@��m@��@�33@�V@��@���@���@�G�@�/@��9@��@�S�@�"�@���@��@���@�O�@��@� �@��w@�S�@��R@�ff@��@��h@�`B@���@���@�1@���@���@��P@�dZ@�dZ@�S�@�ȴ@��+@�ff@�=q@�$�@���@��@�O�@��@���@�A�@�9X@��m@���@��@�;d@��P@���@�@�^5@�J@��#@�@���@�X@�?}@�&�@�%@��j@�Q�@��m@�\)@�S�@�S�@�+@��H@�n�@�E�@�-@���@��7@�G�@�V@��@�A�@�1'@��@��@��@�S�@��@���@���@��+@�^5@���@��@��@��@�hs@�`B@���@��7@��@��/@���@��@�j@��@�r�@��@�t�@�33@��H@��\@�v�@�ff@�=q@�-@�=q@�-@�J@��#@�@���@�G�@�V@���@��`@���@��9@��D@�r�@�I�@��;@��F@�t�@�;d@��y@���@��+@�n�@�=q@�@���@�X@��@��j@�bN@�A�@�9X@�(�@� �@�  @��F@���@�dZ@�K�@�C�@��@��y@���@�n�@�=q@�{@�@��^@�?}@��@��@�V@��@���@��u@�j@�A�@��@��m@���@��P@�K�@��H@��R@�n�@�{@��@���@�p�@�7L@�V@��9@�Z@�1'@� �@�@\)@~��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�VA�XA�^5A�VA�\)A�ZA�VA�O�A�`BA�`BA�XA�1'A�{A�VA���A�  A��mA���A���A�t�A��\A�n�A�M�A�ZA�"�A���A��A��;A�ĜA�ȴA߾wA߲-Aߣ�Aߡ�Aߛ�Aߗ�Aߏ\A߅A߉7A�~�A߅A�~�A�|�A߁A�z�A�~�A�z�A�|�A�|�A�v�A�z�A�z�A�x�A�z�A�t�A�x�A�r�A�t�A�p�A�l�A�p�A�hsA�n�A�hsA�jA�jA�ffA�jA�ffA�dZA�dZA�`BA�ffA�`BA�dZA�dZA�bNA�ffA�^5A�bNA�^5A�`BA�bNA�\)A�`BA�^5A�ZA�^5A�ZA�ZA�\)A�XA�^5A�ZA�VA�ZA�VA�XA�XA�S�A�VA�ZA�XA�VA�ZA�S�A�ZA�VA�VA�VA�O�A�S�A�Q�A�Q�A�VA�O�A�VA�O�A�VA�O�A�VA�S�A�S�A�S�A�M�A�S�A�M�A�Q�A�O�A�O�A�O�A�M�A�O�A�K�A�Q�A�I�A�O�A�I�A�M�A�I�A�M�A�K�A�I�A�M�A�G�A�K�A�K�A�E�A�K�A�I�A�G�A�M�A�E�A�I�A�G�A�G�A�K�A�E�A�I�A�I�A�G�A�K�A�E�A�K�A�K�A�G�A�K�A�I�A�I�A�K�A�E�A�K�A�G�A�G�A�K�A�E�A�K�A�E�A�G�A�G�A�?}A�A�A�A�A�9XA�;dA�;dA�1'A�5?A�-A�{A�A��Aޥ�Aޏ\Aޏ\Aމ7A�(�A�ƨAݍPA�Q�A�=qA��A�bA�A��A��A��mA��AܾwAܩ�Aܡ�A܇+A�p�A�hsA�XA�Q�A�A�A�VA��A��`A��/A��;A��A��A��#A���A��A���A���A���A���A�ƨA���A�ĜA���A���A�ĜA�A�Aۺ^A۸RA۶FAۼjA۸RA۸RAۼjA۶FAۺ^Aۺ^A۶FAۼjAۼjA۸RA۾wAۺ^AۼjA���A۰!Aۗ�Aە�AۓuAۑhAۗ�AۓuAۍPAۍPAۅAۃAۅA�|�A�z�A�t�A�^5A�Q�A�O�A�O�A�A�A�33A�+A�VA�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��TA��A���A���A�ƨAڶFAڙ�AڅA�p�A�ffA�K�A�jA�
=A�A�`BA��A��A���A�AֶFA֣�A֓uA�~�A�Q�A�5?A� �A�"�A�{A�bA�A���A��A��`A���A�ĜAռjAպ^AռjAոRAլA՟�AՋDA�jA�ZA�1'A�{A��`A���AԲ-A�
=AӮA�z�A�E�A�A���A�ƨA�ƨAҼjAҝ�AҋDA�x�A�ffA�`BA�\)A�Q�A�C�A�-A�oA�
=A���A�Q�A��/A�ƨAа!AС�AЕ�AЉ7A�S�A�=qA��A��A�ĜAϬAρA�VA�?}A�1'A���Aβ-A�dZA�S�A�$�AͼjA�S�A� �A���A��
A̼jA̲-ÁA�-A���A˲-Aˏ\A�p�A�A�A���Aʴ9A�5?A�1A��yA��/A���A�ȴAɼjAɮAɬAɣ�Aɝ�Aɟ�Aɟ�Aə�Aɛ�Aɟ�Aɛ�Aɥ�Aɧ�AɬA�~�A��A��
AȾwAȓuA�z�A�jA�+A�JA��A���A�;dAƋDA�5?A�A���AŶFAŃA� �A�1A�ĜAāA�ZA�A�A�+A��A�JA�  A��A��;A��#A���AîAã�AÇ+A�K�A��A�A°!APA�`BA�K�A�"�A���A��DA�dZA�Q�A�G�A�33A��A�1A���A��A���A���A��\A�^5A��A�
=A��A�ĜA���A��hA��A�t�A�ZA�{A��A�O�A�?}A�-A�+A�-A�$�A��A�%A��A���A��RA�C�A�JA��
A��FA��-A���A�z�A�`BA�A�A�+A�JA���A��yA��/A��#A���A���A�A�ȴA�ĜA���A�ĜA��wA��-A���A��hA�n�A�hsA�ffA�^5A�O�A�O�A�G�A�A�A�/A��A�  A��HA���A���A��^A�x�A��-A�5?A��!A�O�A�%A���A��FA���A���A��uA��A�t�A�l�A�S�A�?}A�=qA�7LA�1'A�33A�+A�&�A�&�A�oA���A�v�A� �A��A���A�5?A�JA�1A�A�  A���A���A��A��
A��A��A�r�A�O�A�$�A��/A�z�A�1'A�VA��`A��A�r�A�G�A�5?A��A���A���A���A�C�A�VA�  A��A��`A��
A��wA��FA��!A��A���A��DA�z�A�z�A�jA�Q�A�9XA�"�A�A��TA���A��+A�S�A�5?A�$�A� �A��A��A�bA���A��TA���A���A�|�A�`BA�G�A�33A��A��A�oA�
=A�  A���A��mA���A�ĜA���A��hA�ffA��wA��7A�&�A���A���A��7A�x�A�hsA�=qA��A���A��A�A��wA��jA��jA��RA��A���A���A���A���A��\A�~�A�n�A�ZA�E�A��A��TA�A��A�/A�ĜA���A���A���A���A��uA��PA��DA��7A�~�A�t�A�p�A�dZA�K�A�
=A���A���A��DA�|�A�K�A��A��`A���A�z�A�Q�A�+A��A�bA�1A���A��mA��^A���A�t�A�9XA��;A���A�~�A�XA�ƨA��A��A�z�A�p�A�hsA�ZA�XA�M�A�VA�I�A�5?A�  A��yA��FA�~�A��DA��A�|�A� �A�A���A��PA�jA�;dA���A���A��9A���A���A��hA��DA��A�v�A�dZA�VA�E�A�9XA�1'A�-A�{A���A��;A���A�~�A�z�A�r�A�r�A�r�A�v�A�v�A�n�A�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                              111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B2�B2�B0�B49B1�B2�B3�B1�B2-B0�B0�B%�B.�B.�B.�B.�B.B.IB-�B-�B.B.B-�B-�B-�B-�B-�B-�B-�B.B-�B-�B.B-wB-�B-CB,B*�B$�B~B�B%B1�B49B6B7B7�B8�B;�B;0B;0B:^B=�B?}Bj�BqB{�B�1B��B�B�)B�B�B�B�B'RB1'B7�B9�BC�BD�BU2BJ�BGBFtBC�BN<BK^BIBN�BJ�BK^BI�BGEBB[BB�B>�B:�B7B5�B/�B1'B/�B!�B�B�B_BMB��B�AB�ABݘB�}B�RB�wB��B�4B��BxBsMBT,B?�B5B#BBMB@B�B
�B
�oB
�B
�5B
ѷB
��B
�=B
�SB
�JB
r�B
c�B
GB
8RB
1�B
%�B
eB	��B	��B	�#B	��B	�^B	��B	�bB	|B	s�B	dZB	_pB	L�B	B�B	?B	>�B	;dB	3�B	($B	�B	�B	xB	�B��B�B��B�B�vB�jB��B�B՛B��B��B��BŢBȴB�6B�6BбB�BB˒BȴB�6B�aB�dB� B��B�*B��B�eB��B��B�6B�nB�B��B��B�?B�aB�'B�$B�qB�,B��B�B�VB	oB	B	"B	:B	�B	~B	�B	�B	B	�B	oB	{B	B	�B	!bB	%FB	33B	M�B	^5B	w�B	k�B	N�B	(�B	$@B	/B	9�B	@�B	S�B	~�B	�"B	�\B	��B	��B	zxB	��B	��B	��B	��B	��B	�1B	�VB	��B	}�B	w�B	�B	�+B	�{B	��B	��B	�=B	�4B	~(B	~(B	|B	{B	yrB	}"B	cB	�B	�B	��B	�.B	�4B	��B	�:B	�:B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�uB	��B	�MB	��B	�1B	�7B	�B	�B	��B	��B	��B	� B	��B	�uB	�uB	��B	��B	�@B	��B	��B	�VB	�6B	��B	�aB	�?B	�B	��B	��B	�aB	��B	�aB	��B	�_B	�UB	��B	��B	��B	��B	�UB	�UB	��B	�CB	��B	��B	��B	��B	�LB	�B	��B	��B	��B	ĜB	�?B	ŢB	�B	ɆB	��B	ǮB	��B	ȴB	��B	˒B	��B	ԕB	��B	ԕB	��B	��B	خB	��B	�yB	�EB	�B	��B	�B	ںB	ޞB	��B	�B	�B	�B	�B	�B	� B	�TB	�B	�&B	�B	�2B	�B	�"B	�]B	��B	�)B	�/B	��B	��B	� B	�;B	�oB	�iB	� B	�AB	�B	�GB	�B	��B	�B	�B	�vB	�B	�AB	�MB	�B	�B	�+B	��B	�B	�B	�`B	�2B	�8B	�lB	��B	��B	�"B	��B	�PB	�cB	��B	��B	�.B
 4B
�B
�B
�B
�B
+B
+B
_B
_B
�B
_B
�B
	7B

=B

�B

�B

�B

rB

=B
JB
(B
4B
�B
�B
PB
�B
PB
VB
�B
�B
.B
 B
hB
�B
�B
�B
B
�B
oB
�B
B
B
MB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
=B
�B
�B
�B
B
B
 'B
$@B
$�B
%zB
&�B
'RB
'�B
(�B
(�B
(�B
'�B
($B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
*0B
+kB
,=B
.IB
.�B
.�B
/�B
0�B
1'B
1�B
2�B
2�B
3�B
3hB
49B
49B
4nB
4nB
4�B
4�B
5tB
5tB
6FB
6B
6�B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
8RB
8RB
8�B
8�B
9$B
8�B
9�B
:*B
:�B
:�B
:�B
:�B
;dB
<�B
<�B
<�B
=B
=qB
=qB
=<B
=B
<�B
=�B
=qB
=qB
=<B
=B
=B
=B
=B
=qB
>B
>wB
>�B
?B
?�B
?�B
?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B/�B3�B2�B2�B5B/�B1�B2�B/�B2aB0UB1�B1�B7B-�B1�B.}B6�B,qB2aB1�B.B9�B1�B/�B4�B3hB49B/OB2�B.�B3�B/OB0�B.}B/�B/�B
��B0UB.IB/�B-�B/B/�B-wB/�B-�B/OB.IB.IB/�B.IB-�B/OB.B/�B-CB/�B.IB/�B.�B-CB/OB,�B/B-�B,�B.�B-wB-�B.}B-�B.�B,�B.�B-CB.B.IB,�B.�B-B.}B-�B,�B/B-B.B.�B,�B.�B.B-B.�B-B.B.}B,�B.B-�B-CB/B.}B,�B.B.�B,�B.�B,�B.B-�B-wB.�B,�B-�B-�B,�B/B,�B.�B,�B.�B-B-wB-�B-CB.�B,�B.�B-B.B-wB-�B-�B-B.�B,�B.�B,qB/B-B.�B-wB-�B.}B,�B/B-wB-�B/B-B-�B.�B,qB.�B-CB.B.�B,�B/B-CB-B.}B,�B.}B-B-B.}B-B.B.IB,�B.�B,�B-�B.IB,�B.}B,=B.B,�B,qB.�B+�B,=B,qB*�B*�B+B(XB)�B-wB*0B($B6FB#B!�B2aB7�B 'B �B�B�B�BB�B�B�BqBOBIB�BSB�B�B�B#nB$tB&�B1�B-�B0�B2�B1[B2�B33B2�B4nB33B5?B4nB4�B5tB6�B7LB4�B6FB6�B4�B5�B7B5�B7B9$B6�B7�B8�B6�B8�B7�B7LB8�B6�B7LB:�B8�B9$B:�B9$B<�B>�B:^B;dB;�B9�B;0B=<B:�B;�B;dB9XB;dB:�B9�B=�B=<B9�B8�B<6B8�B7B<jB:�B:�B;�B=�B;�B=�B<jB=<B=qB;�B?�B>wB=<B?B>BB<�B@OB=�B>�B=�B>BB;dB8�B;�B>B9$B:^Bv�B��B\]B_�BgmBi�BgmBhsBiDBjBiyBncBqvBp�BrBo5BqvBo5Bu%BncBp�Bq�Bt�Bq�BrBs�BqvBq�Bt�BrGBu�Bv�BwfBzDBz�B}�BzDB~�B�B�:B�4B��B��B��B��B�YB��B�IB��B��B�=B��B��B�	B��B��B��B�B��B� B�OB�B�6B�B��B��B��B��B�3B�RB�B�LB��B�dB��B�^B�BB�B��B�HB�B�pB�B��B�)BɺB͟B�0B��B�5B�B��BیB�#B��B�HB��B�DB��B�B�B��B�sB�B�B�
B�B�B�B�B�B��B�>B�B�B��B�B�BuB�lB��B�lB�vB�B�cB�GB�2B�>B$B�BB�BBeB"4B�B�B#�B(�B*eB&�B(�B*�B)_B($B+B)�B'B)*B.IB(�B,B3�B8�B:�B4nB:�B8�B4�B>BCaB:�B2�B49B2-B4�B6�B5tB3�B5�B8B6�B>B;dBB�B:�B>�BA�B@OB<�B>�B>B?�BGzBQ�BJ�BD3BE�BB[B@�B@�B@�BB�BM�BqvBp�BL0BR BS[BL�BJXBN<BK�BL�BJ#BI�BNBFtBJ�BI�BG�BF�BHBIRBEmBFBGzBD�BF�BGzBGEBF�BHBF�BC�BD3BEBC-BB'BC-BHBE9BD�BCaBB�B?}BB�BMBgBPHBZQBLdBP�BN<BK^BK)BJXBJ#BJ�BJ�BJ#BK�BK�BHKBIBIRBF�BH�BF�BD�BG�BS�BO�BOBBO�BR�BUgBNpBI�BJ�BJXBIRBIRBI�BMjBMBK�BE�BJXBI�BNBOBBIRBE�BG�BO�BM6BI�BGEBK�BE�BI�BI�BJ#BG�BC�BB[BC�BD�BB�BA�BA�BB'BC�BDgB@�B>�BA�BB�BD3BB�B@�BD3BC-BEmBC�B@OB>wB=�B<�B=B:�B=�B>�B9�B<6B:�B8�B7�B6�B6�B6zB8�B8RB7�B6B7B7LB5�B5�B0�B6�B@�B5�B:*B:�B*�B,�B+kB-CB/�B.IB2�B3hB/�B.�B/B/�B2�B2�B1�B1�B1�B0�B2-B1�B2�B1'B.�B0�B,qB*�B*0B'B(�B�BOB=BeB�B�B�B$B�BeB�BFB�B=BB:B�B
	B�BPB�B(B�B
rBBABABuBB�B�B �B{B�BxBoB��B�B B�+B��B��B�B�B�fB�B�B�cB��B�AB��B�B�fB��B��B�B�KB�BیB��B�#B��B��B�]B�B�,B�[BҽB�vBϫB�B�<B��B�jB�6B�RB�?B�B��B��B��B�mB�dB��B�B��B��B��B�XB�jB��4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                              444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                              444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022092603084920220926030849IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022100522321720221005223217QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022100522321720221005223217QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194720230210131947IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                